def reading(f):
    def process_lines(callback):
        line_no = 1
        with open('input.txt', 'r') as f:
            try:
                for line in f:
                    result = callback(line)
                    if result != None:
                        return result
                    line_no += 1
            except ValueError:
                raise Error(f'invalid value in input file at line: {line_no}')

    def wrapped():
        return f(process_lines)

    return wrapped


@reading
def solve1(with_each_line):
    pairs = set()
    def foreach_line(line):
        number = int(line)
        complement = 2020 - number
        if number in pairs:
            return number * complement
        pairs.add(complement)
    return with_each_line(foreach_line)


@reading
def solve2(with_each_line):
    numbers_so_far = []
    sums = dict()
    def foreach_line(line):
        number = int(line)
        if number in sums:
            other_two = sums[number]
            return number * other_two[0] * other_two[1]
        for past_num in numbers_so_far:
            sums[2020 - past_num - number] = (number, past_num)
        numbers_so_far.append(number)
    return with_each_line(foreach_line)


# both of these assume there is exactly one solution
if __name__ == '__main__':
    print(f'solution1: {solve1()}')
    print(f'solution2: {solve2()}')
