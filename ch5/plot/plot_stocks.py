#!/usr/bin/env python3

import matplotlib
# sets the backend for matplotlib to 'Agg' which is
# Anti-Grain Geometry, a high-quality rendering engine for C++
# Useful for generating static images without displaying on screen
matplotlib.use('Agg')

from datetime import datetime
import matplotlib.pyplot as plt

class Stock():
    def __init__(self, filename):
        self.time = []
        self.open = []
        self.high = []
        self.low = []
        self.close = []
        self.adjclose = []
        # [1:] is slice notation, meaning exclude first element (line) b/c it's heading
        data = [line.strip() for line in open(filename, 'r').readlines()][1:]
        for line in data[::-1]:
            line = line.split(',')
            self.time.append(datetime.strptime(line[0], '%Y-%m-%d'))
            self.open.append(float(line[1]))
            self.high.append(float(line[2]))
            self.low.append(float(line[3]))
            self.close.append(float(line[4]))
            self.adjclose.append(float(line[5]))

datapath = '../data2018'

stocks = ['AAPL', 'AMZN', 'CRAY', 'CSCO', 'HPQ',
          'IBM', 'INTC', 'MSFT', 'NVDA', 'ORCL']

for stock in stocks:
    s = Stock(datapath + '/'+stock+'.csv')
    fig = plt.figure(figsize=(12,6))
    ax = fig.add_subplot(111, ylim=(0, max(s.adjclose)))
    plt.plot(s.time, s.adjclose, 'k-', lw=1)
    plt.title(stock, fontsize=16)
    plt.ylabel('Adj. close [USD]', fontsize=16)
    plt.grid(True)
    plt.savefig(stock + '.png', dpi=100)
    plt.savefig(stock + '.svg')
    plt.close(fig)