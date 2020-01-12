import base64
from lib import to_safe_base64, from_safe_base64


def test_safe_base64():
    '''
    c.f. https://en.wikipedia.org/wiki/Base64#Base64_table to see which binary patterns match to which characters
    '''
    # TT+/
    b1 = base64.b64encode(bytes([0x4D, 0x3F, 0xBF]))
    assert 'TT-_' == to_safe_base64(b1)
