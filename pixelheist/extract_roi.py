import cv2
import json
from operator import itemgetter

img = cv2.imread('ss_browser_window_with_data.png', cv2.IMREAD_UNCHANGED)
with open('coords.json') as f:
    coords = json.load(f)
x, y, w, h = itemgetter('x','y','w','h')(coords)
roi = img[y:y+h, x:x+w] 
cv2.imwrite('roi.png', roi)