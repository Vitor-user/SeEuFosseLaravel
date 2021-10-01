import Questao1
import TestLib

main :: IO()
main =
    do
        test1 <- avaliar "4 8 3 * +"
        assertEqual test1 28 "\"4 8 3 * +\" should be igual to 28"

        test2 <- avaliar "4 8 + 3 *"
        assertEqual test2 36 "\"4 8 + 3 *\" should be igual to 36"

        test3 <- avaliar "46 8 4 * 2 / +"
        assertEqual test3 62 "\"46 8 4 * 2 / +\" should be igual to 62"

        test4 <- avaliar "2 1 4 2 1 + * 3 + + *"
        assertEqual test4 32 "\"2 1 4 2 1 + * 3 + + *\" should be igual to 32"

        test5 <- avaliar "5.0 4.0 + 2 /"
        assertEqual test5 4.5 "\"5.0 4.0 + 2 /\" should be igual to 4.5"

        test5 <- avaliar "5.0 4.0 -"
        assertEqual test5 9 "\"5.0 4.0 + 2 /\" should be different to 9"
