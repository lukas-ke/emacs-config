"""Read an image from the given path and store it on the clipboard."""
import sys
import win32clipboard
from pathlib import Path
from io import BytesIO
from PIL import Image
import PIL

ERROR_FILE_NOT_SPECIFIED = 10
ERROR_FILE_NOT_FOUND = 20
ERROR_PIL = 30

def _send_to_clipboard(image):
    output = BytesIO()
    image.convert('RGB').save(output, 'BMP')
    data = output.getvalue()[14:]
    output.close()

    win32clipboard.OpenClipboard()
    win32clipboard.EmptyClipboard()
    win32clipboard.SetClipboardData(win32clipboard.CF_DIB, data)
    win32clipboard.CloseClipboard()


def image_file_to_clipboard(file_path):
    """Copy the given image file to the clipboard."""
    with Image.open(file_path) as image:
        _send_to_clipboard(image)


def _run():
    if len(sys.argv) != 2:
        return ERROR_FILE_NOT_SPECIFIED

    file_path = Path(sys.argv[1])
    return image_file_to_clipboard(file_path)


if __name__ == '__main__':
    try:
        result = _run()
        sys.exit(result)
    except FileNotFoundError:
        sys.exit(ERROR_FILE_NOT_FOUND)
    except PIL.UnidentifiedImageError:
        sys.exit(ERROR_PIL)
