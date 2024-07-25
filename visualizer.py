import pygame
import sys

if len(sys.argv) != 4:
    print("Usage: python visualizer [maze_100_100.txt] [maze.png] [Block_size]")
    sys.exit()

INPUT_FILENAME = sys.argv[1]
OUTPUT_FILENAME = sys.argv[2]
BLOCK_SIZE = int(sys.argv[3])

if BLOCK_SIZE < 1 or BLOCK_SIZE > 100:
    print("Invalid Block size, choose an integer between 2 and 100")
    sys.exit()

WHITE = (255, 255, 255)
BLACK = (0, 0, 0)

with open(INPUT_FILENAME, "r") as file:
    maze = [list(line.strip()) for line in file]

rows = len(maze)
cols = len(maze[0])


pygame.init()
SCREEN_WIDTH = cols * BLOCK_SIZE
SCREEN_HEIGHT = rows * BLOCK_SIZE

screen = pygame.display.set_mode((SCREEN_WIDTH, SCREEN_HEIGHT))
pygame.display.set_caption("Maze")
screen.fill(WHITE)


for row in range(rows):
    for col in range(cols):
        if maze[row][col] == '#':
            pygame.draw.rect(screen, BLACK, (col * BLOCK_SIZE, row * BLOCK_SIZE, BLOCK_SIZE, BLOCK_SIZE))


pygame.display.flip()
pygame.image.save(screen, OUTPUT_FILENAME)
