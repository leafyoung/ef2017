import binpack_simple
from package import Package

def binpack(packages, bin=None, iterlimit=5000):
    return binpack_simple.binpack(packages, bin, iterlimit)

def test(func):
    import time
    from package import Package
    fd = open('testdata.txt')
    vorher = 0
    nachher = 0
    start = time.time()
    counter = 0
    bin = Package("370x270x250")

    for line in fd:
        counter += 1
        if counter > 450:
            break
        packages = [Package(pack) for pack in line.strip().split()]
        if not packages:
            continue
        bins, rest, bxyz = func(packages, bin=bin)
        if rest:
            print "invalid data", rest, line
        else:
            vorher += len(packages)
            nachher += len(bins)
        print bin
        print bins
        print len(bins)
        print bins
        print bxyz
        print rest
        break


if __name__ == '__main__':
    print "py",
    test(binpack)
