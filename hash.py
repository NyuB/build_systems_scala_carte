from hashlib import sha256
import sys

def help_this():
    print(f"Usage: {sys.argv[0]} <filename>\n\tPrints the SHA-256 hash of <filename>")

if __name__ == '__main__':
    if len(sys.argv) < 2:
        help_this()
        exit(1)
    
    h = sha256()
    with open(sys.argv[1], 'rb') as f:
        content = f.read()
        h.update(content)
    print(h.hexdigest())

