from collections import deque
txt = deque(int(x) for x in open('input08.txt').read().split(" "))

def parse():
    nb_children, nb_metadata = txt.popleft(), txt.popleft()
    childs, metadata = [parse() for _ in range(nb_children)], [int(txt.popleft()) for _ in range(nb_metadata)]
    return sum([childs[i-1][0] for i in metadata if 1 <= i <= len(childs)] if childs else metadata), sum(metadata)+sum(x[1] for x in childs)

print("p1 {}\np2 {}".format(*parse()))
