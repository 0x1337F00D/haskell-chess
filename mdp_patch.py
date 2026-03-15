import re

with open('src/Chess/Engine/Search/AlphaBeta.hs', 'r') as f:
    content = f.read()

# Add mate distance pruning logic at the beginning of alphaBetaBody
# after `let inCheck = ...`
mdp_logic = """
        -- Mate Distance Pruning
        let mateScore = mateValue - scPly ctx
        let alpha' = max alpha (-mateScore)
        let beta'  = min beta (mateScore - 1)
        if alpha' >= beta'
        then return alpha'
        else do
"""

# Find where to insert
incheck_pattern = r"let inCheck = case checkState of InCheck -> True; NotInCheck -> False\n"
content = content.replace(incheck_pattern, incheck_pattern + mdp_logic)

# Re-indent the rest of the alphaBetaBody function block by 4 spaces.
# It starts from `ttEntry <- probeTT tt hash` until the `where` block.

# First, isolate the block to indent
probeTT_pattern = r"(        ttEntry <- probeTT tt hash\n)"
parts = content.split("        ttEntry <- probeTT tt hash\n")

if len(parts) > 1:
    block_to_indent = "        ttEntry <- probeTT tt hash\n" + parts[1]

    # We only want to indent up to the `  where` block.
    where_parts = block_to_indent.split("  where\n")
    if len(where_parts) > 1:
        body_to_indent = where_parts[0]

        # Indent everything in body_to_indent by 4 spaces
        lines = body_to_indent.split("\n")
        indented_lines = ["    " + line if line.strip() else line for line in lines]

        # Replace alpha and beta with alpha' and beta' in the indented block
        indented_body = "\n".join(indented_lines)

        # Let's write a simple regex replacement to replace word boundaries of alpha and beta
        indented_body = re.sub(r'\balpha\b', "alpha'", indented_body)
        indented_body = re.sub(r'\bbeta\b', "beta'", indented_body)

        # Special case: alphaBeta function call itself shouldn't be alpha'Beta
        indented_body = indented_body.replace("alpha'Beta", "alphaBeta")

        # Put everything back together
        new_block = indented_body + "  where\n" + where_parts[1]
        content = parts[0] + new_block

with open('src/Chess/Engine/Search/AlphaBeta.hs', 'w') as f:
    f.write(content)
print("Patch applied")
