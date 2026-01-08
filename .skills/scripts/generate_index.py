import os
import re
from pathlib import Path

# Use relative path from script location to root of .skills
SCRIPT_DIR = Path(__file__).resolve().parent
SKILLS_DIR = SCRIPT_DIR.parent
INDEX_FILE = SKILLS_DIR / "INDEX.md"

def parse_frontmatter(file_path):
    with open(file_path, 'r', encoding='utf-8') as f:
        content = f.read()
    
    match = re.search(r'^---\n(.*?)\n---', content, re.DOTALL)
    if not match:
        return None, content
    
    yaml_block = match.group(1)
    metadata = {}
    
    # Basic parsing (handling simple lists like triggers is tricky in regex, doing simple parse)
    lines = yaml_block.split('\n')
    current_key = None
    
    for line in lines:
        line = line.rstrip()
        if not line: continue
        
        if line.lstrip().startswith('- ') and current_key == 'triggers':
            val = line.split('- ', 1)[1].strip('"\'')
            if isinstance(metadata[current_key], list):
                metadata[current_key].append(val)
        elif ':' in line:
            key, val = line.split(':', 1)
            key = key.strip()
            val = val.strip()
            if val == "":
                current_key = key
                metadata[key] = []
            else:
                current_key = None
                metadata[key] = val
                
    return metadata, content

def generate_index():
    print(f"Scanning skills in {SKILLS_DIR}...")
    categories = {
        'core': [],
        'project': [],
        'workflow': []
    }
    
    priority_order = {'critical': 0, 'high': 1, 'medium': 2, 'low': 3}

    for root, dirs, files in os.walk(SKILLS_DIR):
        # Exclude internal directories
        if 'scripts' in root or 'templates' in root or 'registry' in root:
            continue
            
        for file in files:
            if file.endswith(".md") and not file.startswith("_") and file != "INDEX.md" and file != "README.md" and file != "AI_GUIDE.md" and file != "GUIDELINES.md" and file != "CHANGELOG.md":
                path = Path(root) / file
                meta, content = parse_frontmatter(path)
                
                if meta and 'category' in meta:
                    cat_raw = meta.get('category', '').lower()
                    cat_main = cat_raw.split('/')[0] if '/' in cat_raw else cat_raw
                    
                    if cat_main in categories:
                        try:
                            rel_path = str(path.relative_to(SKILLS_DIR)).replace('\\', '/')
                        except ValueError:
                            rel_path = str(path)
                        
                        has_xml = '<context>' in content and '<instruction>' in content
                        
                        item = {
                            'name': meta.get('name', file),
                            'path': rel_path,
                            'priority': meta.get('priority', 'medium').lower(),
                            'triggers': meta.get('triggers', []),
                            'xml_compliant': has_xml,
                            'subcategory': cat_raw if '/' in cat_raw else None
                        }
                        categories[cat_main].append(item)

    for cat in categories:
        categories[cat].sort(key=lambda x: (priority_order.get(x['priority'], 99), x['name']))

    content = ["# Índice de Skills de Arainfor\n"]
    content.append("**Generado automáticamente. Sistema Next-Gen (Context Enhanced).**\n")
    
    section_titles = {
        'core': 'Core (Fundamentales)',
        'project': 'Proyectos',
        'workflow': 'Workflows'
    }

    for cat_key, items in categories.items():
        if items:
            content.append(f"\n## {section_titles.get(cat_key, cat_key.title())}")
            for item in items:
                prio_icon = ""
                if item['priority'] == 'critical': prio_icon = "🔴 **CRÍTICO**"
                elif item['priority'] == 'high': prio_icon = "Rx **ALTA**"
                
                xml_badge = "✨" if item['xml_compliant'] else ""
                
                content.append(f"- {xml_badge} [{item['name']}]({item['path']}) {prio_icon}")
                
                if item['triggers']:
                    triggers_str = ", ".join([f"`{t}`" for t in item['triggers'][:3]])
                    if len(item['triggers']) > 3: triggers_str += "..."
                    content.append(f"  - *Triggers: {triggers_str}*")

    with open(INDEX_FILE, 'w', encoding='utf-8') as f:
        f.write('\n'.join(content))
    
    print(f"Index generated at {INDEX_FILE}")

if __name__ == "__main__":
    generate_index()
