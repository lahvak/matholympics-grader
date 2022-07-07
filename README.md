# matholympics

Program for grading answer sheets for the Math Olympics competition at SVSU.

```
Usage: matholympics-exe [-d|--data CSVFILE] [-c|--codes|--schools CODEFILE]
                        [--mca1 MCAFILE] [--mca2 MCAFILE] [--datadir DATADIR]
                        [--TeXdir TEXDIR]
  Grade Math Olympics data and produce reports

Available options:
  -d,--data CSVFILE        CSV data file from SCANTRON
                           machine (default: "data.csv")
  -c,--codes,--schools CODEFILE
                           File containing schools and school
                           codes (default: "codes.txt")
  --mca1 MCAFILE           MCA (correct answers) file for level
                           1 (default: "level1.mca")
  --mca2 MCAFILE           MCA (correct answers) file for level
                           2 (default: "level2.mca")
  --datadir DATADIR        Directory containing data files. Defaults to the
                           current working directory.
  --TeXdir TEXDIR          Directory where the generated TeX files should be
                           placed. Defaults to the current working directory.
  -h,--help                Show this help text
```

## Input files:

### The data file

A file produced by the SCANTRON machine, containing raw data from the answer sheets in CSV format.

### School codes

A list of school names and codes.

A text file with each line corresponding to one school.  Each line contains the school name and a three digit code, separated by comma.

### MCA files

Correct answers for multiple choice questions.

Each MCA file is a text file where each line corresponds to one question. Each line consists of either

*   a list of uppercase letters A through E.  In this case, __any__ of these letters is considered correct. One or more of them must be marked, however, _no other letters_ should be marked in order for the answer to be considered correct.
*   an _exclamation mark_ (!) followed by a list of uppercase letters A through E. In this case, _exactly these letters_ must be marked on the SCANTRON sheet for the answer to be considered correct.

The following is a valid example of a MCA file with 25 questions:

```
D
BC
B
B
BDE
C
E
D
!CE
C
B
C
B
D
D
B
D
ABD
A
B
B
C
!AC
A
B
```
For the first question, D is the correct answer. For the second question, B or C are correct, so marking either one or both of them will be graded as correct, but marking any other letter will be incorrect. For the ninth question, exactly the letters C and E must be marked.
