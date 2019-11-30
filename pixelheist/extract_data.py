import cv2
import numpy as np
import json

img = cv2.imread('ss_browser_window_no_data.png', cv2.IMREAD_UNCHANGED)

# accessing a pixel is as easy as accessing the x-y coordinate: img[x,y]
# the array returned though is in BGR, not RGB

def convertBGRToHexScale(val):
    (b, g, r) = val
    s = b + g + r
    