#!/usr/bin/env python

import json
from matplotlib import pyplot
import sys
import subprocess

def get_branching_factors(cookedlines):
    p = subprocess.Popen(
        ['../annotate', '--auto', '--branching-factors'],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )
    out = p.communicate(input='\n'.join(cookedlines).encode('utf-8'))
    decoded_out = (out[0].decode('utf-8'), out[1].decode('utf-8'))
    return p.wait() != 0, decoded_out


def process_cooked_file(infile_name, raw_numbers):
    gameno = infile_name.replace('.cooked', '')
    with open(infile_name, 'r') as f:
        lines = f.readlines()

    failed, out = get_branching_factors(lines)
    try:
        factors = [int(line) for line in out[0].splitlines()]
    except:
        failed = True
    if failed:
        print ('%s: annotate --auto --branching-factors complained:' % infile_name)
        print (out)
    else:
        outfile_name = infile_name.replace('.cooked', '.bfs')
        with open(outfile_name, 'w') as f:
            f.write(out[0])
        print ('%s: Wrote %d factors to %s' % (infile_name, len(factors), outfile_name))
        raw_numbers[gameno] = factors


def process_bfs_file(infile_name, raw_numbers):
    gameno = infile_name.replace('.bfs', '')
    with open(infile_name, 'r') as f:
        lines = f.readlines()
    try:
        factors = [int(line) for line in lines]
        raw_numbers[gameno] = factors
    except:
        print ('%s: bad numeric input:' % infile_name)
        print (lines)


if __name__ == '__main__':
    raw_numbers = {}
    for name in sys.argv[1:]:
        if name.endswith('.bfs'):
            process_bfs_file(name, raw_numbers)
        elif name.endswith('.cooked'):
            process_cooked_file(name, raw_numbers)
        elif name.endswith('.json'):
            with open(name, 'r') as f:
                raw_numbers = json.load(f)

    if len(sys.argv) >= 2:
        with open('branching-factors.json', 'w') as f:
            text = json.dumps(raw_numbers)
            text = text.replace('{"', '{\n"')
            text = text.replace('], ', '],\n')
            text = text.replace(']}', ']\n}')
            f.write(text)

    fig = pyplot.figure()
    ax = fig.add_subplot()
    ax.set_yscale('log', base=10)
    xs = []
    ys = []
    for gameno, factors in raw_numbers.items():
        xs += list(range(len(factors)))
        ys += factors
    ax.set_xlabel('Move')
    ax.set_ylabel('Branching factor')
    ax.scatter(xs, ys, c='blue', s=1)
    pyplot.show()
