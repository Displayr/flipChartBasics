context("Match Table")

ref.table <- matrix(1:24, 6, 4, dimnames = list(letters[1:6], LETTERS[1:4]))
rev.table <- matrix(1:35, 7, 5, dimnames = list(letters[7:1], LETTERS[5:1]))
v10 <- structure(1:10, .Names = letters[10:1])

cols <- c(Feminine = "#FF0000", `Health-conscious` = "#E87D85", Innocent = "#E87D85", 
    Older = "#BED6E3", `Open to new experiences` = "#F2A09A", Rebellious = "#F3A9A1", 
    Sleepy = "#E87D85", Traditional = "#87BAE7", `Weight-conscious` = "#0000FF")
xmat <- structure(c(6.125, 2, 10.5, 64.625, 22.375, 25.5, 9.5, 91.25, 
    0.5, 57.125, 57.75, 21.625, 22.5, 8.875, 4.75, 23.25, 14.625, 
    76.125, 22.375, 53.5, 11.375, 5.375, 50.625, 64, 9.75, 3, 63.875, 
    8.875, 2.5, 10, 39, 16.75, 17.75, 13.5, 54.75, 0, 61.5, 57.875, 
    44.625, 9.875, 16.625, 3.75, 29.75, 3.75, 76.625, 9.375, 30.625, 
    6.875, 6.75, 49.25, 44.75, 5.5, 4.375, 40.375, 9.25, 17.375, 
    29.875, 7.25, 12.875, 15.25, 38.875, 2.5, 5.75), .Dim = c(9L, 
    7L), .Dimnames = list(c("Feminine", "Health-conscious", "Innocent", 
    "Older", "Open to new experiences", "Rebellious", "Sleepy", "Traditional", 
    "Weight-conscious"), c("Coke", "Diet Coke", "Coke Zero", "Pepsi", 
    "Diet Pepsi", "Pepsi Max", "None of these")), statistic = "%", name = "q5 - duplicate", questions = c("q5 - duplicate", 
    "SUMMARY"))
xnames <- rev(rownames(xmat))

custom.palette <- structure(c("#FF0000", "#2E2E2E", "#464646", "#5D5D5D", "#747474", 
    "#8B8B8B", "#A2A2A2", "#B9B9B9", "#D1D1D1", "#E8E8E8"), .Names = c("Coca-Cola", 
    "Diet Coke", "Coke Zero", "Pepsi ", "Diet Pepsi", "Pepsi Max", 
    "Dislike all cola", "Don't care", "NET", NA))
brand.names <- c("Coca-Cola", "Diet Coke", "Coke Zero", "Pepsi", "Diet Pepsi", 
    "Pepsi Max", "Dislike all cola", "Don't care")

vals <- structure(c(72, 16, 23, 7, 27, 15, 160, 62, 26, 37, 11, 21, 10, 
        167, 134, 42, 60, 18, 48, 25, 327), .Dim = c(7L, 3L), statistic = "n", .Dimnames = list(
            c("Coca Cola", "Diet Coke", "Coke Zero", "Pepsi Light", "Pepsi Max", 
            "Pepsi", "NET"), c("Male", "Female", "NET")), name = "Global frequentCola - Categorical by Q2. Gender", questions = c("Global frequentCola - Categorical", 
        "Q2. Gender [Visualization - Standard R Charts.sav]"))
pd <- structure(c(1, 3, 16, 2, 34, 13, 14, 51, 46, 34, 62, 45, 21, 
        96, 61, 45, 85, 60, 46, 62, 66, 101, 89, 81, 137, 75, 80, 120, 
        40, 78, 108, 40, 58, 25, 17, 50), .Dim = c(6L, 6L), .Dimnames = list(
            c("Coca Cola", "Diet Coke", "Coke Zero", "Pepsi", "Pepsi Light", 
            "Pepsi Max"), c("Don t Know", "Hate", "Dislike", "Neither like nor dislike", 
            "Like", "Love")), statistic = "n", name = "Q6. Brand preference", questions = c("Q6. Brand preference", 
        "SUMMARY"), assigned.rownames = TRUE)

test_that("Match table",
{
   res <- MatchTable(v10, ref.table)
   expect_equal(res, 10:5, check.attributes = FALSE)
   
   expect_error(MatchTable(v10[7:10], ref.table, x.table.name = "Color values"), "Color values: Missing values for 'e', 'f'")
   expect_error(MatchTable(1:6, ref.table), NA)
   expect_equal(MatchTable(cols, xmat), cols, check.attributes = FALSE)
   
   expect_error(res <- MatchTable(cols, ref.names = xnames), NA)
   expect_equal(res[1], "#0000FF", check.attributes = FALSE)
   
   expect_error(MatchTable(custom.palette, ref.names = brand.names), NA)
   
   expect_warning(MatchTable(unname(custom.palette), ref.names = letters[1:10]), NA)
   expect_warning(res <- MatchTable(custom.palette, ref.names = c()), "Names were ignored")
   expect_equal(length(res), 1)
   
   expect_error(res <- MatchTable(rev.table*10, ref.table), NA)
   expect_equal(dimnames(res), dimnames(ref.table))
   
   colnames(rev.table) <- NULL
   rownames(rev.table) <- NULL
   expect_error(res <- MatchTable(rev.table, ref.table), NA)
   expect_equal(res, rev.table[1:6, 1:4], check.attributes = FALSE)
  
   # check output if table dimensions don't match 
   expect_warning(MatchTable(vals, ref.table = pd), "Only column 'NET' was used")
   expect_error(MatchTable(vals, ref.table = pd[,1:2]), "Values should either be a single-column table or have the same column names as the input data")
})

test_that("Ignore case and trim whitespace",
{
    expect_error(MatchTable(rev.table, ref.names = LETTERS[1:4], ignore.case = FALSE),
        "Missing values for 'A', 'B', 'C', 'D'")
    expect_equal(MatchTable(rev.table, ref.names = c("A ", " B", "C")),
        structure(c(7L, 6L, 5L, 14L, 13L, 12L, 21L, 20L, 19L, 28L, 27L, 
        26L, 35L, 34L, 33L), .Dim = c(3L, 5L), .Dimnames = list(c("a", 
        "b", "c"), c("E", "D", "C", "B", "A"))))
})

test_that("Duplicated row names",
{
    df <- structure(list(A = c(1, 1, 3), B = 4:6, C = 7:9, D = 10:12), row.names = c("a", 
"a ", "a   "), class = "data.frame")
    expect_warning(MatchTable(df, ref.names = "a"))
    m <- MatchTable(df, ref.names = "a ", trim.whitespace = FALSE)
    expect_equal(m, structure(c(1, 5, 8, 11), .Dim = c(1L, 4L), .Dimnames = list(
    "a ", c("A", "B", "C", "D"))))
})

df <- structure(list(v1 = c(0.287577520124614, 0.788305135443807, 0.4089769218117, 
    0.883017404004931, 0.940467284293845, 0.0455564993899316, 0.528105488047004, 
    0.892419044394046, 0.551435014465824, 0.456614735303447, 0.956833345349878, 
    0.453334156190977, 0.677570635452867, 0.572633401956409, 0.102924682665616, 
    0.899824970401824, 0.24608773435466, 0.0420595335308462, 0.327920719282702, 
    0.954503649147227, 0.889539316063747, 0.6928034061566, 0.640506813768297, 
    0.994269776623696, 0.655705799115822, 0.708530468167737, 0.544066024711356, 
    0.59414202044718, 0.28915973729454, 0.147113647311926, 0.963024232536554, 
    0.902299045119435, 0.690705278422683, 0.795467417687178, 0.0246136845089495, 
    0.477795971091837, 0.758459537522867, 0.216407935833558, 0.318181007634848, 
    0.231625785352662, 0.142800022382289, 0.414546335814521, 0.413724326295778, 
    0.368845450924709, 0.152444747742265, 0.13880606344901, 0.233034099452198, 
    0.465962450252846, 0.265972640365362, 0.857827715342864, 0.0458311666734517, 
    0.442200074205175, 0.798924845643342, 0.12189925997518, 0.560947983758524, 
    0.20653138961643, 0.127531650243327, 0.753307864302769, 0.895045359153301, 
    0.374462775886059, 0.665115194628015, 0.0948406609240919, 0.383969637798145, 
    0.27438364457339, 0.814640038879588, 0.448516341391951, 0.810064353048801, 
    0.812389509519562, 0.794342321110889, 0.439831687603146, 0.754475158639252, 
    0.629221131559461, 0.710182401351631, 0.000624773325398564, 0.475316574098542, 
    0.220118885161355, 0.379816537722945, 0.612771003274247, 0.351797909243032, 
    0.111135424347594, 0.243619472719729, 0.66805558744818, 0.417646779678762, 
    0.788195834029466, 0.102864644257352, 0.434892741497606, 0.984956979984418, 
    0.893051114398986, 0.886469060787931, 0.175052650272846, 0.130695691565052, 
    0.653101925039664, 0.343516472261399, 0.656758127966896, 0.320373242488131, 
    0.187691119266674, 0.782294301316142, 0.0935949867125601, 0.46677904156968, 
    0.511505459900945), v2 = c(0.386757359839976, 0.0355316237546504, 
    0.813215732574463, 0.966335338540375, 0.843689900357276, 0.685617603361607, 
    1.73296662932262, 0.910216101910919, 1.0675297472626, 1.92768666381016, 
    1.54918308416381, 0.417752697132528, 0.617573665454984, 1.94268490048125, 
    1.16980018699542, 1.52164725074545, 0.745418788865209, 1.53838782245293, 
    1.0753543660976, 1.82799089932814, 0.370592883788049, 0.564436834771186, 
    0.189924826379865, 0.420974158216268, 1.9541979925707, 0.592604350764304, 
    1.45196605380625, 1.57137566851452, 0.210835492238402, 0.4791892580688, 
    0.541089744772762, 0.202116988133639, 0.235827682539821, 1.98247311171144, 
    1.97210859460756, 0.274134942796081, 1.81061916332692, 1.15260367514566, 
    0.79089771816507, 0.899604968260974, 1.4130038022995, 0.165005491580814, 
    0.678625160362571, 1.36157510243356, 0.633898496162146, 1.66313719609752, 
    0.430344165302813, 0.995897872373462, 0.552099345251918, 0.384046637453139, 
    1.90124252811074, 0.643451075535268, 0.956912767607719, 0.0559851438738406, 
    1.09491893602535, 1.28848044108599, 1.19252708926797, 0.643874751403928, 
    1.78222862491384, 1.25251389481127, 0.605809830594808, 0.776409327518195, 
    0.320950184948742, 1.72510379506275, 1.90620242757723, 1.12728936690837, 
    0.659094826783985, 1.99323443742469, 0.469639351591468, 1.22534393053502, 
    0.21635705884546, 0.974065139889717, 0.198891646694392, 0.32233152538538, 
    0.565985741559416, 1.1677446719259, 1.46341531770304, 0.331041822209954, 
    1.73293552640826, 1.41714827436954, 1.52079907245934, 0.294168217107654, 
    0.716113926842809, 1.34666496282443, 1.04764520330355, 0.699603585526347, 
    0.481061403173953, 0.11638359259814, 0.473239484243095, 1.78015582635999, 
    1.62365483213216, 1.49503264436498, 0.309823451563716, 0.249484177213162, 
    1.94945161696523, 0.872259992174804, 0.928033253643662, 0.33059616247192, 
    1.16987311467528, 0.541556038893759), months = structure(c(1L, 
    1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
    1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 
    2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
    2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 
    3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 
    3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 4L, 4L, 4L, 4L, 4L, 4L, 
    4L, 4L, 4L), .Label = c("January 2016", "February 2016", "March 2016", 
    "April 2016", "May 2016", "June 2016", "July 2016", "August 2016", 
    "September 2016", "October 2016", "November 2016", "December 2016"
    ), class = c("ordered", "factor"))), scatter.variable.indices = c(x = 1, 
    y = 2, sizes = NA, colors = 3, groups = NA), row.names = c(NA, 
    100L), class = "data.frame")

test_that("Empty groups",
{
    expect_equal(GetNumColors(df, "Scatter", scatter.colors.column = 3)$num.series, 12)
})
                                                                                                                                                             
    