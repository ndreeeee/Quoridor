let gameState = null;
let difficulty = 'easy';
let wallOrientation = 'h'; // 'h' or 'v'

const cellSize = 50;
const gapSize = 10;

function startGame(level) {
    difficulty = level;
    document.getElementById('modal').style.display = 'none';

    fetch('/game/start')
        .then(res => {
            if (!res.ok) throw new Error('Server Error: ' + res.statusText);
            return res.json();
        })
        .then(data => {
            gameState = data;
            renderBoard();
        })
        .catch(err => {
            alert("Error starting game: " + err.message + "\nCheck Prolog console for details.");
        });
}

function toggleOrientation() {
    wallOrientation = wallOrientation === 'h' ? 'v' : 'h';
    document.getElementById('toggle-orientation').innerText =
        wallOrientation === 'h' ? 'Horizontal' : 'Vertical';
}

function renderBoard() {
    const board = document.getElementById('board');
    board.innerHTML = ''; // Clear

    // Render Cells
    for (let y = 1; y <= 9; y++) {
        for (let x = 1; x <= 9; x++) {
            const cell = document.createElement('div');
            cell.className = 'cell';
            cell.dataset.x = x;
            cell.dataset.y = y;
            cell.onclick = () => handleMove(x, y);

            // Render Pawns
            if (gameState.p1.x === x && gameState.p1.y === y) {
                const p1 = document.createElement('div');
                p1.className = 'pawn p1';
                cell.appendChild(p1);
            }
            if (gameState.p2.x === x && gameState.p2.y === y) {
                const p2 = document.createElement('div');
                p2.className = 'pawn p2';
                cell.appendChild(p2);
            }

            // Highlight Valid Moves
            if (gameState.valid_moves && gameState.turn === 'p1') {
                const isValid = gameState.valid_moves.some(m => m[0] === x && m[1] === y);
                if (isValid) {
                    cell.classList.add('valid-move');
                }
            }

            board.appendChild(cell);
        }
    }

    // Render Walls
    gameState.walls.forEach(w => {
        const wall = document.createElement('div');
        wall.className = w.o === 'h' ? 'wall-h' : 'wall-v';
        // Calculate position in pixels relative to board
        // Grid gap logic is tricky in CSS grid, absolute positioning is easier for walls
        // Cell (x,y) top-left is:
        // Left: (x-1)*(cellSize+gap) + gap
        // Top: (y-1)*(cellSize+gap) + gap

        const left = (w.x - 1) * (cellSize + gapSize) + gapSize;
        const top = (w.y - 1) * (cellSize + gapSize) + gapSize;

        if (w.o === 'h') {
            wall.style.left = left + 'px';
            wall.style.top = (top + cellSize) + 'px'; // In the gap below the row
        } else {
            wall.style.left = (left + cellSize) + 'px'; // In the gap right of the col
            wall.style.top = top + 'px';
        }
        board.appendChild(wall);
    });

    // Render Wall Placement Hitboxes (Gaps)
    // We need clickable areas between cells.
    // Horizontal gaps: between row Y and Y+1, spanning 2 cols (X, X+1)
    // Vertical gaps: between col X and X+1, spanning 2 rows (Y, Y+1)

    for (let y = 1; y <= 8; y++) {
        for (let x = 1; x <= 8; x++) {
            // We can place a wall at (x,y)
            const hitbox = document.createElement('div');
            hitbox.className = wallOrientation === 'h' ? 'gap-h' : 'gap-v';

            const left = (x - 1) * (cellSize + gapSize) + gapSize;
            const top = (y - 1) * (cellSize + gapSize) + gapSize;

            if (wallOrientation === 'h') {
                hitbox.style.left = left + 'px';
                hitbox.style.top = (top + cellSize) + 'px';
            } else {
                hitbox.style.left = (left + cellSize) + 'px';
                hitbox.style.top = top + 'px';
            }

            hitbox.onclick = (e) => {
                e.stopPropagation();
                handleWall(x, y);
            };
            board.appendChild(hitbox);
        }
    }

    // Update Stats
    document.getElementById('p1-walls').innerText = gameState.p1.walls;
    document.getElementById('p2-walls').innerText = gameState.p2.walls;
    document.getElementById('status').innerText =
        gameState.turn === 'p1' ? "Your Turn" : "AI Thinking...";
}

function handleMove(x, y) {
    if (gameState.turn !== 'p1') return;
    sendAction({ type: 'move', x: x, y: y });
}

function handleWall(x, y) {
    if (gameState.turn !== 'p1') return;
    sendAction({ type: 'wall', x: x, y: y, o: wallOrientation });
}

function sendAction(move) {
    fetch('/game/move', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
            difficulty: difficulty,
            state: gameState,
            move: move
        })
    })
        .then(res => {
            console.log('Response status:', res.status);
            return res.json();
        })
        .then(data => {
            console.log('Response data:', data);
            console.log('data.valid:', data.valid);
            if (data.valid) {
                gameState = data.state;
                renderBoard();
                if (data.winner) {
                    setTimeout(() => alert(data.winner === 'player' ? "You Win!" : "AI Wins!"), 100);
                }
            } else {
                alert("Invalid Move!");
            }
        })
        .catch(err => {
            console.error('Fetch error:', err);
            alert("Error: " + err.message);
        });
}
