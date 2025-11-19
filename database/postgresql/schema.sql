-- ============================================================================
-- ONLAYN TEST SİSTEMİ - POSTGRESQL SCHEMA
-- ============================================================================

-- Əgər cədvəllər varsa, silib yenidən yaradaq (development üçün)
DROP TABLE IF EXISTS test_results CASCADE;
DROP TABLE IF EXISTS test_questions CASCADE;
DROP TABLE IF EXISTS tests CASCADE;
DROP TABLE IF EXISTS questions CASCADE;
DROP TABLE IF EXISTS students CASCADE;
DROP TABLE IF EXISTS test_centers CASCADE;

-- ============================================================================
-- 1. TEST MƏRKƏZLƏRİ
-- ============================================================================
CREATE TABLE test_centers (
    center_id SERIAL PRIMARY KEY,
    center_name VARCHAR(200) NOT NULL,
    region VARCHAR(100),
    city VARCHAR(100),
    address TEXT,
    computer_count INTEGER DEFAULT 30,
    is_active BOOLEAN DEFAULT TRUE,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- ============================================================================
-- 2. ŞAGİRDLƏR
-- ============================================================================
CREATE TABLE students (
    student_id SERIAL PRIMARY KEY,
    first_name VARCHAR(100) NOT NULL,
    last_name VARCHAR(100) NOT NULL,
    fin_code VARCHAR(7) UNIQUE NOT NULL,
    birth_date DATE,
    gender VARCHAR(10),
    school_name VARCHAR(200),
    class_level INTEGER,
    center_id INTEGER REFERENCES test_centers(center_id),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- ============================================================================
-- 3. SUALLAR BAZASI
-- ============================================================================
CREATE TABLE questions (
    question_id SERIAL PRIMARY KEY,
    sual_id VARCHAR(20) UNIQUE NOT NULL,  -- MAT_001, AZE_001, ENG_001
    fenn VARCHAR(50) NOT NULL,            -- Fənn
    mezmun_sahesi VARCHAR(100) NOT NULL,  -- Məzmun sahəsi
    sual_metni TEXT NOT NULL,             -- Sual mətni
    variant_a TEXT NOT NULL,
    variant_b TEXT NOT NULL,
    variant_c TEXT NOT NULL,
    variant_d TEXT NOT NULL,
    variant_e TEXT NOT NULL,
    dogru_cavab CHAR(1) NOT NULL,         -- A, B, C, D, E
    
    -- IRT Parametrləri
    cetinlik NUMERIC(5,2),                -- b parameter
    ayirdetme NUMERIC(5,2),               -- a parameter
    texminetme NUMERIC(5,2),              -- c parameter
    fisher_info NUMERIC(5,2),             -- Fisher information
    
    -- Əlavə məlumatlar
    sekil_var BOOLEAN DEFAULT FALSE,
    sekil_yolu TEXT,
    muellif VARCHAR(100),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    qeydler TEXT,
    
    -- İndekslər
    CONSTRAINT chk_dogru_cavab CHECK (dogru_cavab IN ('A', 'B', 'C', 'D', 'E'))
);

-- ============================================================================
-- 4. TESTLƏR
-- ============================================================================
CREATE TABLE tests (
    test_id SERIAL PRIMARY KEY,
    test_adi VARCHAR(200) NOT NULL,
    test_kodu VARCHAR(50) UNIQUE NOT NULL,  -- TEST_2025_001
    test_tarixi DATE,
    baslama_vaxti TIME,
    bitis_vaxti TIME,
    muddet_deqiqe INTEGER DEFAULT 60,       -- Test müddəti (dəqiqə)
    
    -- Test məlumatları
    umumi_sual_sayi INTEGER,
    riyaziyyat_sayi INTEGER DEFAULT 0,
    azerbaycan_sayi INTEGER DEFAULT 0,
    ingilis_sayi INTEGER DEFAULT 0,
    
    -- Status
    status VARCHAR(20) DEFAULT 'hazırlanır',  -- hazırlanır, aktiv, bitib
    is_active BOOLEAN DEFAULT TRUE,
    created_by VARCHAR(100),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    qeydler TEXT
);

-- ============================================================================
-- 5. TEST SUALLARI (Test və Sual əlaqəsi)
-- ============================================================================
CREATE TABLE test_questions (
    id SERIAL PRIMARY KEY,
    test_id INTEGER REFERENCES tests(test_id) ON DELETE CASCADE,
    question_id INTEGER REFERENCES questions(question_id),
    sira_nomresi INTEGER NOT NULL,         -- Testdə sualın sırası (1, 2, 3...)
    
    UNIQUE(test_id, question_id),
    UNIQUE(test_id, sira_nomresi)
);

-- ============================================================================
-- 6. TEST NƏTİCƏLƏRİ (Ən əhəmiyyətli cədvəl)
-- ============================================================================
CREATE TABLE test_results (
    result_id SERIAL PRIMARY KEY,
    test_id INTEGER REFERENCES tests(test_id),
    student_id INTEGER REFERENCES students(student_id),
    center_id INTEGER REFERENCES test_centers(center_id),
    
    -- Test məlumatları
    baslama_vaxti TIMESTAMP,
    bitis_vaxti TIMESTAMP,
    test_muddeti_saniye INTEGER,           -- Şagirdin sərf etdiyi vaxt
    
    -- Cavablar (JSON formatında)
    cavablar JSONB,                        -- {"1": "A", "2": "B", "3": "C", ...}
    
    -- Nəticələr
    umumi_dogru INTEGER DEFAULT 0,
    umumi_sehv INTEGER DEFAULT 0,
    umumi_bos INTEGER DEFAULT 0,
    umumi_bal NUMERIC(5,2) DEFAULT 0,
    
    -- Fənnlərə görə
    riyaziyyat_dogru INTEGER DEFAULT 0,
    riyaziyyat_sehv INTEGER DEFAULT 0,
    riyaziyyat_bos INTEGER DEFAULT 0,
    riyaziyyat_bal NUMERIC(5,2) DEFAULT 0,
    
    azerbaycan_dogru INTEGER DEFAULT 0,
    azerbaycan_sehv INTEGER DEFAULT 0,
    azerbaycan_bos INTEGER DEFAULT 0,
    azerbaycan_bal NUMERIC(5,2) DEFAULT 0,
    
    ingilis_dogru INTEGER DEFAULT 0,
    ingilis_sehv INTEGER DEFAULT 0,
    ingilis_bos INTEGER DEFAULT 0,
    ingilis_bal NUMERIC(5,2) DEFAULT 0,
    
    -- IRT nəticələri
    irt_theta NUMERIC(5,2),                -- Qabiliyyət parametri
    irt_se NUMERIC(5,2),                   -- Standart xəta
    
    -- Sistem məlumatları
    ip_address VARCHAR(50),
    komputer_adi VARCHAR(100),
    synced_to_server BOOLEAN DEFAULT FALSE,  -- Serverə göndərilib?
    sync_time TIMESTAMP,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    
    UNIQUE(test_id, student_id)
);

-- ============================================================================
-- İNDEKSLƏR (Sürət üçün)
-- ============================================================================

-- Suallar üzrə
CREATE INDEX idx_questions_fenn ON questions(fenn);
CREATE INDEX idx_questions_mezmun ON questions(mezmun_sahesi);
CREATE INDEX idx_questions_cetinlik ON questions(cetinlik);

-- Testlər üzrə
CREATE INDEX idx_tests_status ON tests(status);
CREATE INDEX idx_tests_tarix ON tests(test_tarixi);

-- Nəticələr üzrə
CREATE INDEX idx_results_test ON test_results(test_id);
CREATE INDEX idx_results_student ON test_results(student_id);
CREATE INDEX idx_results_center ON test_results(center_id);
CREATE INDEX idx_results_sync ON test_results(synced_to_server);

-- ============================================================================
-- KOMMENTLƏR (İzahlar)
-- ============================================================================

COMMENT ON TABLE questions IS 'Suallar bazası - bütün testlər üçün suallar';
COMMENT ON TABLE tests IS 'Testlərin ümumi məlumatları';
COMMENT ON TABLE test_questions IS 'Test və sual əlaqəsi';
COMMENT ON TABLE test_results IS 'Şagirdlərin test nəticələri';
COMMENT ON TABLE students IS 'Şagird məlumatları';
COMMENT ON TABLE test_centers IS 'Test mərkəzləri';

-- ============================================================================
-- UĞURLA TAMAMLANDI
-- ============================================================================