# Contributing to zCOBOL

Have you been bored lately? If you know COBOL and assembler or Java, or C
there is a job on the zCOBOL project waiting for you. The pay is poor ($0)
but the self actualization rewards can be very satisfying. And there is always
the possibility of future paying jobs helping companies use zCOBOL.

If you are an assembler or COBOL developer who would like to contribute
to the zCOBOL open source project, join the [zCOBOL group](???)
and indicate your specific interests. All users are welcome and are encouraged to submit
bug reports and requests for priority on future open source zCOBOL
and z390 development.

With the recent addition of z390 structured conditional macro assembler extensions,
the development of zCOBOL became feasible and has evolved rapidly. As the
article in the z/System Journal titled, "Easy COBOL Modernization for SOA"
by L. H. Couch and Charles F. Townsend, November 2008 indicates there is
a growing demand for tools such as zCOBOL to help seamlessly bridge legacy
and modern IT solutions.

Current jobs available include writing COBOL verb macros for currently unsupported
verbs. Optimizing the code generation macros
to produce more efficient code and optional code based on
[zCOBOL options](../user_guide/zCOBOL/zCOBOL_options.md) such as
TRUNC, R64, etc. In addition, major effort is still required to covert the
HLASM code generation macros to generated java, C, or MASM.

For COBOL programmers there is the constant need to extend
the zCOBOL regression tests (zCOBOL_Demos_and_Regression_Tests.htm)
written in zCOBOL which verify that zCOBOL statements
produce the expected results. And finally there is a need to develop documentation
on the zCOBOL project as it evolves.

## NIST ANSI 1985 Test Suite Results

v1.5.00a came from RPI 1001 for conditional 88 support, RPI 1002 SET and index
support, and RPI 1012 miscellaneous syntax error corrections. Once some of the
remaining critical support items such as COMPUTE are completed, these numbers
should continue to significantly improve. The plan is to achieve 100% within
the next few releases of zCOBOL and then provide optional regression test
download for the NIST test suite for zCOBOL.

More details regarding NIST validation can be found in the [zCOBOL user guide](../user_guide/zCOBOL/zCOBOL_NIST_COBOL_1985_Test_Results.md).

| Description                       | V1.5.00 | V1.5.00a | Notes                                                |
|-----------------------------------|---------|----------|------------------------------------------------------|
| NIST programs with parsing errors | 140     | 43       | RPI 1012 corrections to zc390 parser                 |
| NIST Programs Compiled            | 319     | 416      | RPI 1012 corrections to zc390 parser                 |
| Total minutes                     | 26      | 44       | 33% increase in number of programs compiled          |
| RC=0 No errors                    | 11      | 12       | most programs are still missing one or more items    |
| RC=8 MNOTE support warning        | 19      | 151      | Warning for unsupported items pending implementation |
| RC=16 At least 1 error message    | 249     | 160      | Error messages from mz390 or az390 macro assembler   |

## Implementation Documentation

The following documents describe recent COBOL statement implementations in detail,
including normative references to [FIPS PUB 21-2](https://nvlpubs.nist.gov/nistpubs/Legacy/FIPS/fipspub21-2.pdf) (ANSI X3.23-1985):

- [REPLACE Statement](./replace_implementation.md) - Compiler-directing text substitution per FIPS PUB 21-2 Section XII
- [SORT Statement](./sort_implementation.md) - Sort-Merge module implementation per FIPS PUB 21-2 Section XI
