raw8 = open("Day8/input8").readlines()
inputs = list(r.split("|")[0].strip().split(" ") for r in raw8)
output = list(r.split("|")[1].strip().split(" ") for r in raw8)

res = list()
for ids in range(len(inputs)):
    tor = dict()
    torbase = dict()
    inputs_o = [i for i in inputs[ids] if len(i) in [2,3,4,7]]
    for c in inputs_o:
        cset = set(c)
        cc = tuple(sorted(list(cset)))
        if len(c) == 2:
            tor[cc] = "1"
            torbase["1"] = cset
        elif len(c) == 3:
            tor[cc] = "7"
        elif len(c) == 4:
            tor[cc] = "4"
            torbase["4"] = cset
        elif len(c) == 7:
            tor[cc] = "8"
            torbase["8"] = cset
    inputs_o = [i for i in inputs[ids] if len(i) not in [2,3,4,7]]
    for c in inputs_o:
        cset = set(c)
        cc = tuple(sorted(list(cset)))
        if len(c) == 6:
            if not (torbase.get("1") <= cset):
                tor[cc] = "6"
                torbase["6"] = cset
            elif (not (torbase.get("4") <= cset)):
                tor[cc] = "0"
            else:
                tor[cc] = "9"
        if len(c) == 5:
            if torbase.get("1") <= cset:
                tor[cc] = "3"
            elif (set.union(cset, torbase["4"]) == torbase["8"]):
                tor[cc] = "2"
            else:
                tor[cc] = "5"
    res.append(tor)
    
res2 = ["", ] * len(output)
for ii, o in enumerate(output):
    for e in o:
        res2[ii] = res2[ii] + res[ii][tuple(sorted(list(set(e))))]
        
sum([int(i) for i in res2])
