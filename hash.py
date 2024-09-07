from hashlib import sha256, md5, sha1
import sys

def help_this():
    print(f"Usage: {sys.argv[0]} <algorithm> <filename>\n\tPrints the hash of <filename> using <algorithm>")

if __name__ == '__main__':
    if len(sys.argv) < 3:
        help_this()
        exit(1)
    
    
    if sys.argv[1] == "md5":
        h = md5()
    elif sys.argv[1] == "sha256":
        h = sha256()
    elif sys.argv[1] == "sha1":
        h = sha1()
    else:
        help_this()
        exit(1)

    with open(sys.argv[2], 'rb') as f:
        content = f.read()
        h.update(content)
    print(h.hexdigest())
