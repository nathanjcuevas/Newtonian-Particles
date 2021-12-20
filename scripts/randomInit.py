import argparse
from random import randrange

parser = argparse.ArgumentParser()

parser.add_argument('--n', default=10, help = 'poll for test')
args = parser.parse_args()

dmin = -225
dmax = 225
vmin = -120
vmax = 120

n = int(args.n)

for _ in range(n):
    xrand = randrange(dmin, dmax)
    yrand = randrange(dmin, dmax)
    vxrand = randrange(vmin, vmax)
    vyrand = randrange(vmin, vmax)
    print(str(xrand) + ',' +
          str(yrand) + ',' + 
          str(vxrand) + ',' + str(vyrand))

