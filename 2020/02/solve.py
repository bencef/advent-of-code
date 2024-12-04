from collections import namedtuple, Counter

Password = namedtuple('Password', ['min', 'max', 'letter', 'password'])

def reading(f):
    def process_lines(callback):
        line_no = 1
        with open('input.txt', 'r') as f:
            try:
                for line in f:
                    password = parse_line(line)
                    valid = callback(password)
                    if valid:
                        yield password
                    line_no += 1
            except ValueError:
                raise ValueError(f'invalid value in input file at line: {line_no}')

    def wrapped():
        return f(process_lines)

    return wrapped


def read_num(input):
    valid_digits = '0123456789'
    digits = []
    for c in input:
        if c in valid_digits:
            digits.append(c)
        else:
            return int(''.join(digits)), len(digits)
    raise ValueError('Empty input')


def parse_line(line):
    min, skip = read_num(line)
    index = skip + 1
    max, skip = read_num(line[index:])
    index += skip + 1
    letter = line[index:index+1]
    index += 3
    password = line[index:]
    return Password(min, max, letter, password)


@reading
def solve1(with_each_line):
    def foreach_line(pw):
        c = Counter(pw.password)
        occurences = c[pw.letter]
        return occurences >= pw.min and occurences <= pw.max
    return len(list(with_each_line(foreach_line)))


@reading
def solve2(with_each_line):
    def xor(a, b):
        return (a and not b) or (b and not a)
    def foreach_line(pw):
        id1 = pw.password[pw.min-1] == pw.letter
        id2 = pw.password[pw.max-1] == pw.letter
        return xor(id1, id2)
    return len(list(with_each_line(foreach_line)))


if __name__ == '__main__':
    print(f'solution1: {solve1()}')
    print(f'solution2: {solve2()}')
