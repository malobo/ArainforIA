import os
import re
import yaml
from pathlib import Path

# Relative paths
SCRIPT_DIR = Path(__file__).resolve().parent
SKILLS_DIR = SCRIPT_DIR.parent
OUTPUT_FILE = SKILLS_DIR / "registry" / "tools.yaml"

def parse_frontmatter(content):
    match = re.search(r'^---\n(.*?)\n---', content, re.DOTALL)
    if not match:
        return None
    return yaml.safe_load(match.group(1))

def parse_inputs(content):
    """
    Extracts inputs from the '## Inputs' section of the markdown.
    Expected format:
    - **name** (type): Description
    """
    inputs = {}
    
    # regex to find the Inputs section
    section_match = re.search(r'## Inputs\s*\n(.*?)(?=\n## |$)', content, re.DOTALL)
    if not section_match:
        return inputs # Return empty dict if no inputs section

    section_content = section_match.group(1)
    
    # regex to parse individual lines: - **name** (type): description
    # Handles optional type or missing type
    input_pattern = re.compile(r'-\s*\*\*(.*?)\*\*\s*(?:\((.*?)\))?:\s*(.*)')
    
    for line in section_content.split('\n'):
        line = line.strip()
        if not line: continue
        
        match = input_pattern.match(line)
        if match:
            name = match.group(1).strip()
            type_str = match.group(2).strip() if match.group(2) else "string"
            desc = match.group(3).strip()
            
            inputs[name] = {
                "type": type_str,
                "description": desc
            }
    
    return inputs

def main():
    print(f"Scanning skills in {SKILLS_DIR}...")
    
    tools_spec = {
        "openSpec": "1.0.0",
        "info": {
            "title": "ArainforIA Skills Registry",
            "version": "1.0.0"
        },
        "components": {
            "tools": {}
        }
    }
    
    tools_count = 0
    
    for root, dirs, files in os.walk(SKILLS_DIR):
        # Exclude internal directories
        if 'scripts' in root or 'registry' in root or 'templates' in root:
            continue
            
        for file in files:
            if file.endswith(".md") and not file.startswith("_") and file not in ["README.md", "INDEX.md", "AI_GUIDE.md", "GUIDELINES.md", "CHANGELOG.md"]:
                
                path = Path(root) / file
                try:
                    with open(path, 'r', encoding='utf-8') as f:
                        content = f.read()
                except Exception as e:
                    print(f"Error reading {file}: {e}")
                    continue

                meta = parse_frontmatter(content)
                if not meta:
                    continue
                
                # Get ID/Name from filename or meta. ideally meta['name'] or filename stem
                tool_id = meta.get('name', path.stem).replace(" ", "-").lower() # normalize id
                
                # Get description: first paragraph after title # Title
                # Or from frontmatter if available (not standard yet but good practice)
                description = "No description provided."
                
                # Rough heuristic for description: line after first # Header
                desc_match = re.search(r'^# .*?\n\n(.*?)\n', content, re.MULTILINE)
                if desc_match:
                    description = desc_match.group(1).strip()
                elif '## Descripción' in content:
                     desc_section = re.search(r'## Descripción\s*\n(.*?)(?=\n## |$)', content, re.DOTALL)
                     if desc_section:
                         description = desc_section.group(1).strip()

                try:
                    rel_uri = str(path.relative_to(SKILLS_DIR)).replace('\\', '/')
                except ValueError:
                    rel_uri = str(path)

                tool_def = {
                    "name": meta.get('name', tool_id),
                    "description": description,
                    "version": str(meta.get('version', '1.0.0')),
                    "labels": [meta.get('category', 'uncategorized')],
                    "implementation": {
                        "type": "markdown",
                        "uri": rel_uri
                    },
                    "inputs": parse_inputs(content)
                }
                
                tools_spec["components"]["tools"][tool_id] = tool_def
                tools_count += 1

    # Ensure registry dir exists
    OUTPUT_FILE.parent.mkdir(parents=True, exist_ok=True)
    
    with open(OUTPUT_FILE, 'w', encoding='utf-8') as f:
        yaml.dump(tools_spec, f, sort_keys=False, allow_unicode=True)
        
    print(f"Generated registry at {OUTPUT_FILE}")
    print(f"Total tools indexed: {tools_count}")

if __name__ == "__main__":
    main()
