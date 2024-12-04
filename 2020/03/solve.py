from collections import namedtuple

def reading(f):
    def process_lines(callback):
        line_no = 1
        with open('input.txt', 'r') as f:
            try:
                for line in f:
                    result = callback(line)
                    # if line_no == 4:
                    #     return
                    line_no += 1
            except ValueError:
                raise Error(f'invalid value in input file at line: {line_no}')

    def wrapped():
        return f(process_lines)

    return wrapped


class Count():

    def __init__(self):
        self.count = 0


    def increase(self):
        self.count += 1


    def add(self, amount):
        self.count += amount


    def get(self):
        return self.count


@reading
def solve1(with_each_line):
    count = Count()
    column = Count()
    def foreach_line(line):
        line = line.rstrip() # remove newline
        col_coord = column.get() % len(line)
        # print(f'{line[:col_coord]}[{line[col_coord]}]{line[col_coord:]}')
        if line[col_coord] == "#":
            count.increase()
        column.add(3)
    with_each_line(foreach_line)
    return count.get()


Steps = namedtuple("Steps", ["right", "down"])


@reading
def solve2(with_each_line):
    counts = {
        Steps(right=1, down=1): Count(),
        Steps(right=3, down=1): Count(),
        Steps(right=5, down=1): Count(),
        Steps(right=7, down=1): Count(),
        Steps(right=1, down=2): Count()
    }
    columns = {
        Steps(right=1, down=1): Count(),
        Steps(right=3, down=1): Count(),
        Steps(right=5, down=1): Count(),
        Steps(right=7, down=1): Count(),
        Steps(right=1, down=2): Count()
    }
    steps = list(counts.keys())
    line_no = Count()
    def foreach_line(line):
        line = line.rstrip() # remove newline
        for step in steps:
            if line_no.get() % step.down == 0:
                col_coord = columns[step].get() % len(line)
                # print(f'{line[:col_coord]}[{line[col_coord]}]{line[col_coord:]}')
                if line[col_coord] == "#":
                    counts[step].increase()
                columns[step].add(step.right)
        line_no.increase()

    with_each_line(foreach_line)
    total = 1
    for idx in counts:
        total *= counts[idx].get()
    return total


# both of these assume there is exactly one solution
if __name__ == '__main__':
    print(f'solution1: {solve1()}')
    print(f'solution2: {solve2()}')
