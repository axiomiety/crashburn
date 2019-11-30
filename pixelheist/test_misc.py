from extract_data import convertBGRToHexScale, PALETTE_RGB_WIDTH

def test_scale_conversion():
    assert 0 == convertBGRToHexScale([0,0,0])
    # below threshold
    assert 1 == convertBGRToHexScale([PALETTE_RGB_WIDTH + PALETTE_RGB_WIDTH//2,0,0])
    # above threshold
    assert 2 == convertBGRToHexScale([PALETTE_RGB_WIDTH + PALETTE_RGB_WIDTH//2+1,0,0])
    assert 15 == convertBGRToHexScale([255*3-10,0,0])