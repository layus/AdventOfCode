
from itertools import *

target = 147061

def recipes (n = target):
    i, j = 0, 1
    r = [3, 7]
    for k in r: yield k
    while True:
        new = list(map(int, str(r[i] + r[j])))
        for k in new: yield k
        r.extend(new)
        l = len(r)
        i = (i + 1 + r[i]) % l
        j = (j + 1 + r[j]) % l

print(''.join(map(str, islice(recipes(), target, target+10))))

def find(seq, pattern):
    i = 0
    it = iter(seq)
    result = tuple(islice(it, len(pattern)))
    for elem in it:
        if result == pattern:
            return i
        i += 1
        result = result[1:] + (elem,)

pattern = tuple(map(int, str(target)))
print(find(recipes(), pattern))
