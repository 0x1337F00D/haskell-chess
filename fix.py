with open('src/Chess/Engine/Search/AlphaBeta.hs', 'r') as f:
    lines = f.readlines()

out_lines = []
for i, line in enumerate(lines):
    if line.startswith('        let inCheck = case checkState of InCheck -> True; NotInCheck -> False'):
        out_lines.append(line)
        out_lines.append("\n")
        out_lines.append("        -- Mate Distance Pruning\n")
        out_lines.append("        let mateScore = mateValue - scPly ctx\n")
        out_lines.append("        let alpha' = max alpha (-mateScore)\n")
        out_lines.append("        let beta'  = min beta (mateScore - 1)\n")
        out_lines.append("        if alpha' >= beta'\n")
        out_lines.append("        then return alpha'\n")
        out_lines.append("        else do\n")
        out_lines.append("\n")
    else:
        out_lines.append(line)

# Now we need to indent everything from the line `        ttEntry <- probeTT tt hash` until the line `  where`
start_idx = -1
end_idx = -1
for i, line in enumerate(out_lines):
    if line.startswith('        ttEntry <- probeTT tt hash'):
        start_idx = i
    if start_idx != -1 and line.startswith('  where'):
        end_idx = i
        break

if start_idx != -1 and end_idx != -1:
    for i in range(start_idx, end_idx):
        if out_lines[i].strip():
            # Indent by 4 spaces
            out_lines[i] = "    " + out_lines[i]

            # Replace alpha and beta
            import re
            # Careful with alphaBeta function name
            out_lines[i] = re.sub(r'\balphaBeta\b', 'ALPHABETA_TMP', out_lines[i])
            out_lines[i] = re.sub(r'\balpha\b', "alpha'", out_lines[i])
            out_lines[i] = re.sub(r'\bbeta\b', "beta'", out_lines[i])
            out_lines[i] = out_lines[i].replace('ALPHABETA_TMP', 'alphaBeta')

with open('src/Chess/Engine/Search/AlphaBeta.hs', 'w') as f:
    f.writelines(out_lines)
