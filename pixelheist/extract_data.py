import cv2
import numpy as np
import json
from itertools import chain

img = cv2.imread('roi.png', cv2.IMREAD_UNCHANGED)

# accessing a pixel is as easy as accessing the x-y coordinate: img[x,y]
# the array returned though is in BGR, not RGB

PALETTE_RGB_WIDTH = 47

def convertBGRToHexScale(val):
    (b, g, r) = val
    s = b + g + r
    remainder = s % PALETTE_RGB_WIDTH
    quotient = s // PALETTE_RGB_WIDTH
    if remainder > PALETTE_RGB_WIDTH/2:
        quotient = min(16, quotient+1)
    return quotient

def rescale(image):
    # is this the best way to scale it?
    # small = cv2.resize(image, (0,0), fx=2/3, fy=2/3) 
    resized_image = cv2.resize(image, (1200, 800))
    return resized_image

def blockify(image, width):
    # we need to generate 'blocks' of pixels of width x width
    h, w, channels = image.shape # we don't use channels
    num_rows = h//width
    num_cols = w//width
    print(num_rows, num_cols)
    counter = 0
    scale = width*width

    ret = []

    for r in range(num_rows):
        for c in range(num_cols):
            tally = 0
            for i in image[r*width:(r+1)*width]:
                for j in i[c*width:(c+1)*width]:
                    tally += sum(j)
            # x = sum(sum(sum(v) for v in i[c*width:(c+1)*width]) for i in image[r*width:(r+1)*width])/scale
            ret.append(tally/scale)
    return ret
            
if __name__ == '__main__':
    rimg = rescale(img)
    blockify(rimg, 5)