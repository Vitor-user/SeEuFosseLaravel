module TestQuestao2 where

import Questao2
import TestLib

main :: IO()
main = 
    do
        assertTrue (verificarParenteses "") "\"\" should be valid"

        assertTrue (verificarParenteses "()") "\"()\" should be valid"

        assertTrue (verificarParenteses "(())()(())") "\"(())()(())\" should be valid"

        assertTrue (verificarParenteses "(eu) (ja(nem)) (sei qm) ()(sou)") "\"(eu) (ja(nem)) (sei qm) ()(sou)\" should be valid"

        ------------------------------------------------------------------

        assertFalse (verificarParenteses ")()(") "\")()(\" should be invalid"

        assertFalse (verificarParenteses "()(") "\"()(\" should be invalid"

        assertFalse (verificarParenteses ")()(") "\")()(\" should be invalid"

        assertFalse (verificarParenteses ")()()()(") "\")()()()(\" should be invalid"

