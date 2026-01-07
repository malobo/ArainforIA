import os
import re
import sys
from pathlib import Path

SKILLS_DIR = Path(r"c:\Arainfor\.skills")

def parse_frontmatter(content):
    match = re.search(r'^---\n(.*?)\n---', content, re.DOTALL)
    if not match:
        return None
    
    yaml_block = match.group(1)
    metadata = {}
    for line in yaml_block.split('\n'):
        if ':' in line:
            key, value = line.split(':', 1)
            key = key.strip()
            # Simple list parsing for triggers
            if value.strip() == "":
                metadata[key] = [] 
            else:
                metadata[key] = value.strip()
    
    # Quick hack to capture list items for triggers if they follow
    if 'triggers' in metadata and metadata['triggers'] == []:
        trigger_block = re.search(r'triggers:\s*\n((?:\s+-\s+".*?"\s*\n?)+)', yaml_block)
        if trigger_block:
            triggers = []
            for line in trigger_block.group(1).split('\n'):
                if '-' in line:
                    triggers.append(line.split('-', 1)[1].strip().strip('"'))
            metadata['triggers'] = triggers

    return metadata

def validate_file(path):
    issues = []
    try:
        with open(path, 'r', encoding='utf-8') as f:
            content = f.read()
    except Exception as e:
        return [f"Error reading file: {e}"]

    # 1. Check Frontmatter
    meta = parse_frontmatter(content)
    if not meta:
        issues.append("Missing or invalid YAML frontmatter")
    else:
        required_fields = ['id', 'name', 'version', 'category', 'priority']
        for field in required_fields:
            if field not in meta:
                issues.append(f"Missing required metadata field: {field}")
        
        # Check Triggers
        if 'triggers' not in meta:
             # Warning only for now, or error if we want to be strict
             issues.append("Missing 'triggers' metadata (Recommended)")
        elif not meta['triggers']:
             issues.append("'triggers' field exists but is empty")

    # 2. Check XML Tags (Context Engineering)
    required_tags = ['context', 'instruction', 'examples']
    for tag in required_tags:
        if not re.search(f"<{tag}>.*?</{tag}>", content, re.DOTALL):
            issues.append(f"Missing required XML section: <{tag}>")

    return issues

def main():
    print(f"Validating skills in {SKILLS_DIR}...")
    all_valid = True
    
    for root, dirs, files in os.walk(SKILLS_DIR):
        for file in files:
            if file.endswith(".md") and not file.startswith("_") and file != "index.md" and file != "README.md":
                path = Path(root) / file
                issues = validate_file(path)
                
                if issues:
                    print(f"\n[FAIL] {file}")
                    for issue in issues:
                        print(f"  - {issue}")
                    all_valid = False
                else:
                    # print(f"[OK] {file}")
                    pass

    if all_valid:
        print("\nAll skills passed validation! System is Next-Gen compliant.")
        sys.exit(0)
    else:
        print("\nSome skills failed validation.")
        sys.exit(1)

if __name__ == "__main__":
    main()
