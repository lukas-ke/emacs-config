# Helper script for luk-org-paste-image
#
# Uses pillow to read an image from the clipboard and saves it to the
# given path

from PIL import ImageGrab
import sys
from pathlib import Path

# Result codes for clipboard_image_to_file
IMAGE_WRITTEN_OK = 0
ERROR_FOLDER_NOT_SPECIFIED = 1
ERROR_FOLDER_DOES_NOT_EXIST = 2
ERROR_NO_IMAGE_ON_CLIPBOARD = 3

def get_available_filename(folder):
    files = {f.name for f in folder.iterdir()}
    for i in range(len(files) + 1):
        name = "paste.png" if i == 0 else f"paste-{i}.png"
        if not name in files:
            return folder / name

def clipboard_image_to_file(folder):
    """Save a PNG-image from clipboard named "paste.png" to the given
    folder.

    Returns IMAGE_WRITTEN_OK if successful.

    """
    if not folder.exists():
        return ERROR_FOLDER_DOES_NOT_EXIST

    img = ImageGrab.grabclipboard()

    if img is None:
        return ERROR_NO_IMAGE_ON_CLIPBOARD

    target_path = get_available_filename(folder)
    img.save(target_path, 'png')
    print(target_path.name)
    return IMAGE_WRITTEN_OK

def run():
    if len(sys.argv) != 2:
        return ERROR_FOLDER_NOT_SPECIFIED

    folder_path = Path(sys.argv[1])
    return clipboard_image_to_file(folder_path)

if __name__ == '__main__':
    result = run()
    exit(result)
