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

def blockify(image, width):
    # we need to generate 'blocks' of pixels of width x width
    h, w, channels = image.shape # we don't use channels
    print(f'h: {h}, w: {w}')
    num_rows = h//width
    num_cols = w//width
    print(f'dividing the image in {num_rows} rows and {num_cols} columns')
    counter = 0
    scale = width*width
    block_num = 14
    ret = []
    for r in range(num_rows):
        for c in range(num_cols):
            tally = 0
            for ii, i in enumerate(image[r*width:(r+1)*width]):
                for jj, j in enumerate(i[c*width:(c+1)*width]):
                    if counter == block_num:
                        t = sum(j)
                        #if t > 10:
                        print(f'sum of RGB components > 10 for ({r*width+ii},{c*width+jj}): {j}')
                    tally += sum(j)
            # x = sum(sum(sum(v) for v in i[c*width:(c+1)*width]) for i in image[r*width:(r+1)*width])/scale
            if counter == block_num:
                print(f'average pixel: {tally/scale}')
                identifyBlock(image, r, c, width)
            ret.append(tally/scale)
            counter += 1
    return ret
            
if __name__ == '__main__':
    rimg = rescale(img)
    cv2.namedWindow('output', cv2.WINDOW_NORMAL) 
    cv2.imshow('output',rimg)
    cv2.waitKey(0)
    arr = blockify(rimg, int(sys.argv[1]) if len(sys.argv) > 1 else 5)
    print([convertToPaletteScale(a) for a in arr])