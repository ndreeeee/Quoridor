# Quoridor Game

Gioco Quoridor implementato in SWI-Prolog con server web.

## Prerequisiti

- [SWI-Prolog](https://www.swi-prolog.org/download/stable) installato

## Istruzioni di avvio

1. Apri **SWI-Prolog**.

2. Carica i file Prolog:
   - Vai su **File → Consult** (o premi `Ctrl+O`).
   - Seleziona **`quoridor.pl`** e premi **Apri**. Aspetta che compili.
   - Ripeti per **`server.pl`**.

3. Avvia il server: ?- server(8000).

Se ottieni `true`, il server è attivo!

4. Gioca:
- Apri il browser.
- Vai su **`http://localhost:8000`**.
- Buon divertimento!

## Risoluzione problemi

- **Errore compilazione**: assicurati che i file siano nella stessa cartella di SWI-Prolog.
- **Porta occupata**: cambia porta, es. `server(8080).` e usa `localhost:8080`.
- **Browser non carica**: verifica che il server dia `true` e riprova.

## File principali

- `quoridor.pl`: logica del gioco.
- `server.pl`: server web.
