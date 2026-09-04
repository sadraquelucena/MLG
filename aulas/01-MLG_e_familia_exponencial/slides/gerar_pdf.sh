#!/bin/bash

set -e

# ==================================================
# Descobre automaticamente o único arquivo .qmd
# da pasta
# ==================================================

QMD_FILES=( *.qmd )

if [ "${#QMD_FILES[@]}" -ne 1 ]; then
    echo "Erro: a pasta slides deve conter exatamente um arquivo .qmd."
    echo "Arquivos encontrados:"
    printf '  %s\n' "${QMD_FILES[@]}"
    exit 1
fi

QMD="${QMD_FILES[0]}"

# Remove a extensão .qmd
BASE="${QMD%.qmd}"

HTML="${BASE}.html"
PDF="${BASE}.pdf"

echo ""
echo "======================================"
echo "Gerando materiais dos slides"
echo "======================================"
echo ""
echo "Arquivo-fonte:"
echo "  $QMD"
echo ""
echo "Saídas:"
echo "  $HTML"
echo "  $PDF"
echo ""

# ==================================================
# Verifica se o HTML foi gerado
# ==================================================

if [ ! -f "$HTML" ]; then
    echo "Erro: HTML não encontrado:"
    echo "  $HTML"
    exit 1
fi

# ==================================================
# PDF
# ==================================================

echo "→ Gerando PDF..."

google-chrome \
  --headless \
  --no-sandbox \
  --disable-gpu \
  --virtual-time-budget=20000 \
  --print-to-pdf="$PDF" \
  --no-pdf-header-footer \
  "file://$(realpath "$HTML")?print-pdf"

if [ ! -f "$PDF" ]; then
    echo "Erro: PDF não foi gerado."
    exit 1
fi

echo "  ✓ PDF gerado"

echo ""
echo "======================================"
echo "Concluído!"
echo "======================================"