with open('.jules/bolt.md', 'r') as f:
    lines = f.readlines()

new_lines = []
skip = False
for line in lines:
    if "## 2024-06-21" in line and not skip:
        if "Returning a boxed  from  forces" in lines[lines.index(line) + 1]:
            skip = True
        else:
            new_lines.append(line)
    elif skip:
        if "## 2024-06-21" in line:
            skip = False
            new_lines.append(line)
    else:
        new_lines.append(line)

with open('.jules/bolt.md', 'w') as f:
    for line in new_lines:
        f.write(line)
