# 🎓 Onlayn Test Sistemi

22 lisey və gimnaziya üçün onlayn test sistemi

## 📦 Xüsusiyyətlər

✅ **Admin Panel** - Excel-dən sual yükləmə, test yaratma
✅ **Şagird Tətbiqi** - Oflayn test, avtomatik göndərmə  
✅ **PostgreSQL + SQLite** - Mərkəzi və lokal bazalar
✅ **Avtomatik Nəticə Toplamaq** - PostgreSQL + Excel + Email
✅ **Deployment Paketi** - Bölgələr üçün hazır ZIP

## 📂 Struktur
```
onlayn_test_sistemi/
├── data/questions/          # Excel suallar bazası
├── data/tests/              # Yaradılmış testlər
├── shiny_apps/admin_panel/  # Admin proqramı
├── shiny_apps/test_app/     # Şagird proqramı
└── deployment/bolge_paketi/ # Bölgələr üçün paket
```

## 🚀 Quraşdırma

1. PostgreSQL quraşdır
2. R və RStudio quraşdır
3. Təlimatları oxu: `Tam_Telimat.html`

## 📥 Bölgə Paketi

`deployment/bolge_paketi/` qovluğunu ZIP-lə və bölgələrə göndər.

## 📚 Təlimatlar

- `Tam_Telimat.html` - Admin üçün tam təlimat
- `deployment/bolge_paketi/QURASDIRMA.txt` - Quraşdırma
- `deployment/bolge_paketi/ISTIFADE.txt` - İstifadə

## 🔧 Texniki Məlumatlar

- **Backend:** R Shiny
- **Bazalar:** PostgreSQL + SQLite
- **Deployment:** RDS faylları
- **Avtomatik Göndərmə:** PostgreSQL + Excel + Email

---

📧 Support: Azerbaijan Education Institute
