#!/bin/bash

set -e

echo ""
echo "======================================"
echo "Gerando thumbnails"
echo "======================================"
echo ""

# ==================================================
# THUMBNAILS DOS SLIDES
# ==================================================

find aulas -path "*/slides/*.pdf" -type f | while read -r PDF
do
    BASE="${PDF%.pdf}"
    THUMB="${BASE}.png"

    echo "→ Slide: $PDF"

    pdftoppm \
      -f 1 \
      -singlefile \
      -png \
      -r 120 \
      "$PDF" \
      "$BASE"

    echo "  ✓ $THUMB"
done

# ==================================================
# THUMBNAILS DAS LISTAS DE EXERCÍCIOS
# ==================================================

find aulas -path "*/lista_de_exercicios/*.pdf" -type f | while read -r PDF
do
    BASE="${PDF%.pdf}"
    THUMB="${BASE}.png"

    echo "→ Lista: $PDF"

    pdftoppm \
      -f 1 \
      -singlefile \
      -png \
      -r 120 \
      "$PDF" \
      "$BASE"

    echo "  ✓ $THUMB"
done

echo ""
echo "======================================"
echo "Thumbnails geradas."
echo "======================================"
echo ""