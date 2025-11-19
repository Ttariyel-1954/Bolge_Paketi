# Suallar Bazası

Bu qovluqda test sualları saxlanılır.

## Fayllar:
- `questions_database_full.xlsx` - 45 sual (3 fənn × 5 məzmun × 3 çətinlik)

## Struktur:
- **Fənnlər**: Riyaziyyat, Ana dili, İngilis dili
- **Məzmun sahələri**: 5 fərqli mövzu
- **Çətinlik**: Asan, Orta, Çətin

## İstifadə:
Bu Excel faylı PostgreSQL-ə yüklənir:
\`\`\`bash
Rscript scripts/load_questions_to_postgres.R
\`\`\`
