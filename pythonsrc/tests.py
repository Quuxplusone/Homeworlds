import copy
import libhomeworlds as L
import unittest


class TestLibhomeworlds(unittest.TestCase):

    def test_module_name(self):
        self.assertEqual(L.__name__, 'libhomeworlds')
        with self.assertRaises(AttributeError) as e:
            L.nonexistent_method()
        self.assertEqual(str(e.exception), "module 'libhomeworlds' has no attribute 'nonexistent_method'")

    def test_newGame(self):
        self.assertEqual(L.newGame.__doc__, 'Return a GameState object representing a brand-new game.')
        with self.assertRaises(TypeError) as e:
            L.newGame('xyzzy')
        self.assertEqual(str(e.exception), "newGame() takes no arguments (1 given)")
        s = L.newGame()
        self.assertIsInstance(s, L.GameState)
        self.assertEqual(str(s), '')
        self.assertEqual(repr(s), "libhomeworlds.GameState('')")
        self.assertEqual(s.gameIsOver(), True)


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
        self.assertEqual(str(m), 'pass')
        self.assertEqual(m.toString(), 'pass')
        self.assertEqual(m.toSDGString(), 'pass')
        self.assertEqual(repr(m), "libhomeworlds.WholeMove('pass')")

    def test_catastrophe(self):
        m = L.WholeMove('move y1 from Alpha to Beta; catastrophe yellow at Beta')
        self.assertEqual(str(m), 'move y1 from Alpha to Beta; catastrophe yellow at Beta')
        self.assertEqual(m.toString(), 'move y1 from Alpha to Beta; catastrophe yellow at Beta')
        self.assertEqual(m.toSDGString(), 'move y1 Alpha Beta; catastrophe Beta yellow')
        self.assertEqual(repr(m), "libhomeworlds.WholeMove('move y1 from Alpha to Beta; catastrophe yellow at Beta')")

    def test_unused_sacrifice_actions(self):
        m = L.WholeMove('sac y3 at Alpha; move y1 from Alpha to Beta; catastrophe yellow at Beta')
        self.assertEqual(str(m), 'sacrifice y3 at Alpha; move y1 from Alpha to Beta; catastrophe yellow at Beta')
        self.assertEqual(m.toString(), 'sacrifice y3 at Alpha; move y1 from Alpha to Beta; catastrophe yellow at Beta')
        self.assertEqual(m.toSDGString(), 'sacrifice y3 Alpha; move y1 Alpha Beta; catastrophe Beta yellow; pass; pass')
        self.assertEqual(repr(m), "libhomeworlds.WholeMove('sacrifice y3 at Alpha; move y1 from Alpha to Beta; catastrophe yellow at Beta')")

    def test_discover(self):
        m = L.WholeMove('move y1 from Alpha to Beta (b2)')
        self.assertEqual(m.toString(), 'move y1 from Alpha to Beta (b2)')
        self.assertEqual(m.toSDGString(), 'discover y1 Alpha b2 Beta')
        self.assertEqual(repr(m), "libhomeworlds.WholeMove('move y1 from Alpha to Beta (b2)')")

    def test_missing_pieces(self):
        m = L.WholeMove('catastrophe green at Widget; sac y3; move y1r2 from Alpha to Beta')
        self.assertEqual(m.toString(), 'catastrophe green at Widget; sacrifice y3; move y1 from Alpha to Beta; move r2 from Alpha to Beta')
        with self.assertRaises(ValueError) as e:
            m.toSDGString()
        self.assertEqual(str(e.exception), "move with missing information cannot be stringified into SDG format")


class TestGameState(unittest.TestCase):

    def test_metadata(self):
        self.assertEqual(L.GameState.__doc__, 'A game state.')
        self.assertEqual(L.GameState.apply.__doc__, 'modify this GameState in place by applying a move')
        self.assertEqual(L.GameState.copyApply.__doc__, 'return the new GameState after applying a move')
        self.assertEqual(L.GameState.gameIsOver.__doc__, 'return True if the game is over')
        self.assertEqual(L.GameState.getBestMove.__doc__, 'get the best move for the given player')
        self.assertEqual(L.GameState.toString.__doc__, 'convert to string')
        self.assertEqual(L.GameState.__deepcopy__.__doc__, 'make a deep copy')

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

    def test_impossible(self):
        with self.assertRaises(ValueError) as e:
            L.GameState('Foo (0, b1) b1b1-\nBar (1, r3) -b1\n')
        self.assertEqual(str(e.exception), "text did not parse as a game state")

    def test_trivial(self):
        s = L.GameState('Foo (0,b3r1) g3-\n' + 'Bar(1,b3y2) -r1y2')
        self.assertEqual(str(s), 'Foo (0, r1b3) g3-\nBar (1, y2b3) -r1y2\n')
        self.assertEqual(s.toString(), 'Foo (0, r1b3) g3-\nBar (1, y2b3) -r1y2\n')
        self.assertEqual(repr(s), "libhomeworlds.GameState('Foo (0, r1b3) g3-\\nBar (1, y2b3) -r1y2\\n')")
        s = L.GameState('Foo (0,b3r1) g3-\n' + 'Bar(1,b3y2) -r1y2\n')
        self.assertEqual(str(s), 'Foo (0, r1b3) g3-\nBar (1, y2b3) -r1y2\n')
        self.assertEqual(s.toString(), 'Foo (0, r1b3) g3-\nBar (1, y2b3) -r1y2\n')
        self.assertEqual(repr(s), "libhomeworlds.GameState('Foo (0, r1b3) g3-\\nBar (1, y2b3) -r1y2\\n')")

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
        self.assertEqual(str(e.exception), "argument 2 must be libhomeworlds.WholeMove, not str")
        with self.assertRaises(TypeError) as e:
            s.apply(L.WholeMove('pass'), 0)
        self.assertEqual(str(e.exception), "an integer is required (got type libhomeworlds.WholeMove)")
        with self.assertRaises(ValueError) as e:
            s.apply(2, L.WholeMove('pass'))
        self.assertEqual(str(e.exception), "attacker must be 0 or 1")

    def test_in_universe_exceptions(self):
        s = L.GameState('Foo (0,b3r1) g3-\n' + 'Bar(1,b3y2) -g1y2')
        with self.assertRaises(ValueError) as e:
            s.apply(0, L.WholeMove('sac g3 at Foo'))
        self.assertEqual(str(e.exception), "The move as parsed was disallowed by the rule against self-destruction.")
        with self.assertRaises(ValueError) as e:
            s.apply(0, L.WholeMove('build y1 at Nonexistent'))
        self.assertEqual(str(e.exception), "The move as parsed referred to a nonexistent star system.")
        with self.assertRaises(ValueError) as e:
            s.apply(1, L.WholeMove('move g1 from Bar to Foo (b1)'))
        self.assertEqual(str(e.exception), "The move as parsed tried to create a new star system with the same name as an existing one.")
        with self.assertRaises(ValueError) as e:
            s.apply(1, L.WholeMove('build at Bar'))
        self.assertEqual(str(e.exception), "The move as parsed was incomplete or ambiguous.")
        with self.assertRaises(ValueError) as e:
            s.apply(0, L.WholeMove('build y1 at Foo'))
        self.assertEqual(str(e.exception), "The move as parsed was disallowed by the rules.")

    def test_nonmutating_apply_exceptions(self):
        s = L.GameState('Foo (0,b3r1) g3-\n' + 'Bar(1,b3y2) -r1y2')
        with self.assertRaises(TypeError) as e:
            s.copyApply()
        self.assertEqual(str(e.exception), "function takes exactly 2 arguments (0 given)")
        with self.assertRaises(TypeError) as e:
            s.copyApply(0, 'pass')
        self.assertEqual(str(e.exception), "argument 2 must be libhomeworlds.WholeMove, not str")
        with self.assertRaises(TypeError) as e:
            s.copyApply(L.WholeMove('pass'), 0)
        self.assertEqual(str(e.exception), "an integer is required (got type libhomeworlds.WholeMove)")
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
        with self.assertRaises(ValueError) as e:
            s.getBestMove(2)
        self.assertEqual(str(e.exception), "attacker must be 0 or 1")

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
