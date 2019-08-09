import copy
import libannotate as L
import unittest


class TestLibannotate(unittest.TestCase):

    def test_module_name(self):
        self.assertEqual(L.__name__, 'libannotate')
        with self.assertRaises(AttributeError) as e:
            L.nonexistent_method()
        self.assertEqual(str(e.exception), "module 'libannotate' has no attribute 'nonexistent_method'")


class TestWholeMove(unittest.TestCase):

    def test_metadata(self):
        self.assertEqual(L.WholeMove.__doc__, 'A whole move, possibly consisting of several actions.')

    def test_lack_of_default_constructor(self):
        with self.assertRaises(TypeError) as e:
            L.WholeMove()
        self.assertEqual(str(e.exception), "function takes exactly 1 argument (0 given)")

    def test_unparseable(self):
        with self.assertRaises(ValueError) as e:
            L.WholeMove('xyzzy')
        self.assertEqual(str(e.exception), "text did not parse as a move")
        with self.assertRaises(ValueError) as e:
            L.WholeMove('pass; xyzzy')
        self.assertEqual(str(e.exception), "text did not parse as a move")

    def test_trivial(self):
        m = L.WholeMove('pass')
        self.assertEqual(m.toString(), 'pass')
        self.assertEqual(m.toSDGString(), 'pass')
        self.assertEqual(repr(m), "libannotate.WholeMove('pass')")

    def test_catastrophe(self):
        m = L.WholeMove('move y1 from Alpha to Beta; catastrophe yellow at Beta')
        self.assertEqual(m.toString(), 'move y1 from Alpha to Beta; catastrophe yellow at Beta')
        self.assertEqual(m.toSDGString(), 'move y1 Alpha Beta; catastrophe Beta yellow')
        self.assertEqual(repr(m), "libannotate.WholeMove('move y1 from Alpha to Beta; catastrophe yellow at Beta')")

    def test_unused_sacrifice_actions(self):
        m = L.WholeMove('sac y3 at Alpha; move y1 from Alpha to Beta; catastrophe yellow at Beta')
        self.assertEqual(m.toString(), 'sacrifice y3 at Alpha; move y1 from Alpha to Beta; catastrophe yellow at Beta')
        self.assertEqual(m.toSDGString(), 'sacrifice y3 Alpha; move y1 Alpha Beta; catastrophe Beta yellow; pass; pass')
        self.assertEqual(repr(m), "libannotate.WholeMove('sacrifice y3 at Alpha; move y1 from Alpha to Beta; catastrophe yellow at Beta')")

    def test_discover(self):
        m = L.WholeMove('move y1 from Alpha to Beta (b2)')
        self.assertEqual(m.toString(), 'move y1 from Alpha to Beta (b2)')
        self.assertEqual(m.toSDGString(), 'discover y1 Alpha b2 Beta')
        self.assertEqual(repr(m), "libannotate.WholeMove('move y1 from Alpha to Beta (b2)')")

    def test_missing_pieces(self):
        m = L.WholeMove('catastrophe green at Widget; sac y3; move y1r2 from Alpha to Beta')
        self.assertEqual(m.toString(), 'catastrophe green at Widget; sacrifice y3; move y1 from Alpha to Beta; move r2 from Alpha to Beta')
        with self.assertRaises(ValueError) as e:
            m.toSDGString()
        self.assertEqual(str(e.exception), "move with missing information cannot be stringified into SDG format")


class TestGameState(unittest.TestCase):

    def test_metadata(self):
        self.assertEqual(L.GameState.__doc__, 'A game state.')

    def test_lack_of_default_constructor(self):
        with self.assertRaises(TypeError) as e:
            L.GameState()
        self.assertEqual(str(e.exception), "function takes exactly 1 argument (0 given)")

    def test_unparseable(self):
        with self.assertRaises(ValueError) as e:
            L.GameState('xyzzy')
        self.assertEqual(str(e.exception), "text did not parse as a game state")
        with self.assertRaises(ValueError) as e:
            L.GameState('Foo (b1) r3-y1\nxyzzy\n')
        self.assertEqual(str(e.exception), "text did not parse as a game state")

    def test_trivial(self):
        s = L.GameState('Foo (0,b3r1) g3-\n' + 'Bar(1,b3y2) -r1y2')
        self.assertEqual(s.toString(), 'Foo (0, r1b3) g3-\nBar (1, y2b3) -r1y2\n')
        s = L.GameState('Foo (0,b3r1) g3-\n' + 'Bar(1,b3y2) -r1y2\n')
        self.assertEqual(s.toString(), 'Foo (0, r1b3) g3-\nBar (1, y2b3) -r1y2\n')

    def test_mutating_apply(self):
        s = L.GameState('Foo (0,b3r1) g3-\n' + 'Bar(1,b3y2) -r1y2')
        m = L.WholeMove('build g1')
        result = s.apply(0, m)
        self.assertEqual(result, None)
        self.assertEqual(s.toString(), 'Foo (0, r1b3) g1g3-\nBar (1, y2b3) -r1y2\n')

    def test_nonmutating_apply(self):
        s1 = L.GameState('Foo (0,b3r1) g3-\n' + 'Bar(1,b3y2) -r1y2')
        self.assertEqual(s1.toString(), 'Foo (0, r1b3) g3-\nBar (1, y2b3) -r1y2\n')
        m = L.WholeMove('build g1')
        s2 = s1.copyApply(0, m)
        self.assertEqual(s1.toString(), 'Foo (0, r1b3) g3-\nBar (1, y2b3) -r1y2\n')
        self.assertEqual(s2.toString(), 'Foo (0, r1b3) g1g3-\nBar (1, y2b3) -r1y2\n')

    def test_mutating_apply_exceptions(self):
        s = L.GameState('Foo (0,b3r1) g3-\n' + 'Bar(1,b3y2) -r1y2')
        with self.assertRaises(TypeError) as e:
            s.apply()
        self.assertEqual(str(e.exception), "function takes exactly 2 arguments (0 given)")
        with self.assertRaises(TypeError) as e:
            s.apply(0, 'pass')
        self.assertEqual(str(e.exception), "argument 2 must be libannotate.WholeMove, not str")
        with self.assertRaises(TypeError) as e:
            s.apply(L.WholeMove('pass'), 0)
        self.assertEqual(str(e.exception), "an integer is required (got type libannotate.WholeMove)")
        with self.assertRaises(ValueError) as e:
            s.apply(2, L.WholeMove('pass'))
        self.assertEqual(str(e.exception), "attacker must be 0 or 1")

    def test_nonmutating_apply_exceptions(self):
        s = L.GameState('Foo (0,b3r1) g3-\n' + 'Bar(1,b3y2) -r1y2')
        with self.assertRaises(TypeError) as e:
            s.copyApply()
        self.assertEqual(str(e.exception), "function takes exactly 2 arguments (0 given)")
        with self.assertRaises(TypeError) as e:
            s.copyApply(0, 'pass')
        self.assertEqual(str(e.exception), "argument 2 must be libannotate.WholeMove, not str")
        with self.assertRaises(TypeError) as e:
            s.copyApply(L.WholeMove('pass'), 0)
        self.assertEqual(str(e.exception), "an integer is required (got type libannotate.WholeMove)")
        with self.assertRaises(ValueError) as e:
            s.copyApply(2, L.WholeMove('pass'))
        self.assertEqual(str(e.exception), "attacker must be 0 or 1")

    def test_deepcopy(self):
        s1 = L.GameState('Foo (0,b3r1) g3-\n' + 'Bar(1,b3y2) -r1y2')
        s2 = copy.deepcopy(s1)
        self.assertEqual(s1.toString(), 'Foo (0, r1b3) g3-\nBar (1, y2b3) -r1y2\n')
        self.assertEqual(s2.toString(), 'Foo (0, r1b3) g3-\nBar (1, y2b3) -r1y2\n')
        m = L.WholeMove('build g1')
        s1.apply(0, m)
        self.assertEqual(s1.toString(), 'Foo (0, r1b3) g1g3-\nBar (1, y2b3) -r1y2\n')
        self.assertEqual(s2.toString(), 'Foo (0, r1b3) g3-\nBar (1, y2b3) -r1y2\n')

    def test_getBestMove(self):
        s = L.GameState("""
            Player1 (0, g3y2) g3-
            Blueberry (b3) y1y1g1-
            Timbuktu (g2) -r1g1
            Player2 (1, y2b1) r3-y2y1
        """)
        m = s.getBestMove(0)
        self.assertEqual(m.toString(), 'move y1 from Blueberry to Player2; catastrophe yellow at Player2')

    def test_gameIsOver(self):
        s = L.GameState("""
            Player1 (0, g3y2) g3-
            Blueberry (b3) y1y1g1-
            Timbuktu (g2) -r1g1
            Player2 (1, y2b1) r3-y2y1
        """)
        m = L.WholeMove('move y1 from Blueberry to Player2; catastrophe yellow at Player2')
        self.assertEqual(s.gameIsOver(), False)
        s.apply(0, m)
        self.assertEqual(s.gameIsOver(), True)


if __name__ == '__main__':
    unittest.main()
