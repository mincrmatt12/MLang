#!/usr/bin/python
import pydotplus
import sys

g = pydotplus.Dot(graph_name='code diagram', graph_type='digraph')

def read_instructions():
    # keep reading until we find a label or eof. return the label name of the next or none
    instructions = ""
    labelname = []
    j = False
    for line in sys.stdin:
        line = line[:-1]
        if line.startswith("  "):
            instructions += line + "\n"
        if line.startswith("  jmp"):
            # grab the label
            labelname.append(line.split(".")[1])
            j = True
        if line.startswith("  ifnz"):
            labelname.append(line.split(".")[1])
        if line.startswith("  ret"):
            j = True
        if line.startswith("."):
            if not j:
                labelname.append(line[1:-1])
            return instructions, labelname, line[1:-1]
    return instructions, labelname, None

last_label = "start"
blocks = []

while last_label != None:
    i, n, ll = read_instructions()
    if last_label == "start" and n == []:
        n = (ll,)
    blocks.append([(last_label, tuple(n)),i])
    last_label = ll
print blocks
nodes = []
edges = []
for i in blocks:
    instruct = i[1]
    instruct = instruct.replace(" ", "\\ ")
    instruct = instruct.replace("\n", "\\n")
    instruct = instruct.replace("|", "\\|")
    instruct = instruct.replace("<", "\\<")
    instruct = instruct.replace(">", "\\>")
    instruct = instruct.replace("{", "\\{")
    instruct = instruct.replace("}", "\\}")
    instruct = instruct.replace("\t", "\\t")
    nodes.append(
            pydotplus.Node(name=i[0][0], **{
                'shape': 'record',
                'label': '{<top> ' + i[0][0] + ' | <c> ' + instruct + '}'
            }) if i[0][0] != "start" else pydotplus.Node(name=i[0][0], **{
                'shape': 'record',
                'label': '{<top> ' + i[0][0] + ' | <c> ' + instruct + '}',
                'rank': 'min'
            })
    )
    if i[0][1] is not ():
        for p in i[0][1]:
            edges.append(pydotplus.Edge(i[0][0] + ":c", p + ":top"))

for i in nodes:
    g.add_node(i)
for i in edges:
    g.add_edge(i)

g.write_png("a.png")
