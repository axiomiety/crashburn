from extract_data import convertBGRToHexScale, PALETTE_RGB_WIDTH, blockify


class MockImage(object):

    def __init__(self, arr):
        # this returns height, width, num channels
        self.shape = len(arr), len(arr[0]), 3
        self.arr = arr

    def __getitem__(self, key):
        return self.arr[key]


def test_scale_conversion():
    assert 0 == convertBGRToHexScale([0,0,0])
    # below threshold
    assert 1 == convertBGRToHexScale([PALETTE_RGB_WIDTH + PALETTE_RGB_WIDTH//2,0,0])
    # above threshold
    assert 2 == convertBGRToHexScale([PALETTE_RGB_WIDTH + PALETTE_RGB_WIDTH//2+1,0,0])
    assert 16 == convertBGRToHexScale([255*3-10,0,0])

def test_blockify():
    img1 = [
        [ (1,1,1), (1,1,1) ],
        [ (1,1,1), (1,1,1) ],
    ]
    assert [3, 3, 3, 3] == blockify(MockImage(img1), 1)
    assert [3] == blockify(MockImage(img1), 2)
