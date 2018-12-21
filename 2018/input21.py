A, B, C, D, E = 0, 0, 0, 0, 0

B = 65536
E = 2024736
while True:
    C = B & 255
    E = C + E
    E = E & 16777215
    E = E * 65899
    E = E & 16777215

    #print (B, E)
    if B < 256:
        print ('###', E)
        B = E | 65536
        E = 2024736
    else:
        B = B // 256
