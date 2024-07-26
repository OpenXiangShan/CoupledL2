# we expect no check for data correctness,
# therefore for RAM in L2/L3 DataStorage and TLRAM,
# we just want it to output zero

# ! TODO: these targets require manual check
targets = ['array_5_ext.v', 'array_6_ext.v', 'mem_ext.v']
files = ['./build/' + target for target in targets]

for file in files:
    lines = []
    with open(file, 'r') as f:
        while True:
            line = f.readline()
            lines.append(line)
            if line.startswith(')'):
                break

    line = lines[-2]
    # print(line)
    data_width = int(line.split('[')[1].split(':')[0]) + 1
    # print(data_width)

    lines.append(f"  assign RW0_rdata = {data_width}'b0;\n")
    lines.append(f"endmodule")

    with open(file, 'w') as f:
        f.writelines(lines)