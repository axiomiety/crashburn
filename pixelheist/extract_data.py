import cv2
import numpy as np
import json
from itertools import chain
import sys

img = cv2.imread('roi.png', cv2.IMREAD_UNCHANGED)

# accessing a pixel is as easy as accessing the x-y coordinate: img[x,y]
# the array returned though is in BGR, not RGB

PALETTE_RGB_WIDTH = 47

def convertBGRToHexScale(val):
    (b, g, r) = val
    s = b + g + r
    return convertToPaletteScale(s)

def convertToPaletteScale(val):
    remainder = val % PALETTE_RGB_WIDTH
    quotient = val // PALETTE_RGB_WIDTH
    if remainder > PALETTE_RGB_WIDTH/2:
        # round up
        quotient = min(16, quotient+1)
    return quotient

def rescale(image):
    # is this the best way to scale it?
    # small = cv2.resize(image, (0,0), fx=2/3, fy=2/3) 
    resized_image = cv2.resize(image, (1200, 800))
    return resized_image

def identifyBlock(img, r, c, w):
    # columns are x, rows are y
    x = c*w
    y = r*w
    print(f'drawing a green square at ({x},{y}) with width {w}')
    cv2.rectangle(img, (x, y), (x+w, y+w), (0, 255, 0), 2)
    cv2.namedWindow('output2', cv2.WINDOW_NORMAL)
    cv2.imshow('output2',img)
    cv2.waitKey(0)

def blockify(image, width, block_num=-1):
    # we need to generate 'blocks' of pixels of width x width
    h, w, channels = image.shape # we don't use channels
    print(f'h: {h}, w: {w}')
    num_rows = h//width
    num_cols = w//width
    print(f'dividing the image in {num_rows} rows and {num_cols} columns')
    counter = 0
    scale = width*width
    #block_num = 0
    ret = []
    for r in range(num_rows):
        for c in range(num_cols):
            tally = []
            for ii, i in enumerate(image[r*width:(r+1)*width]):
                for jj, j in enumerate(i[c*width:(c+1)*width]):                    
                    tally.append(sum(j))
            if counter == block_num:
                print(f'average pixel: {sum(tally)/scale}')
                identifyBlock(image, r, c, width)
            ret.append(tally)
            counter += 1
    return ret

def weigh_fn_pyramid(vals, width):
    # this weight function assigns a higher weight at the values
    # in the center, and lower weight at the edges
    # this is only applicable for a width > 2
    if width < 3:
        return vals

    # number of steps to the height of the pyramid
    # the center of the pyramid has weight 1
    def pyramid(n):
        r = np.arange(n)
        d = np.minimum(r,r[::-1])
        return np.minimum.outer(d,d)

    p = pyramid(width)
    num_elems = width*width
    sum_weights = sum(p.reshape((1,num_elems)))
    ret = []
    print(p)
    print(vals)
    for val in vals:
        ret.append(list(np.multiply(np.array(val).reshape((width, width)), p).reshape(1, num_elems)))
    return ret

def weigh(tally, width, weight_fn):
    return [weigh_fn(vals, width) for vals in tally]

if __name__ == '__main__':
    rimg = rescale(img)
    cv2.namedWindow('output', cv2.WINDOW_NORMAL) 
    cv2.imshow('output',rimg)
    cv2.waitKey(0)
    arr = blockify(rimg, int(sys.argv[1]) if len(sys.argv) > 1 else 5)

    print([convertToPaletteScale(a) for a in arr])