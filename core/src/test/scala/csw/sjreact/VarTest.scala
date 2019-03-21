package csw.sjreact

import utest._

object VarTest extends TestSuite {

  val tests = Tests {
    implicit val scope = new WatchScope {}
    "watcher" - {
      "should be informed when state changed" - {
        val state = Var.state[String]("123")

        var symbol = ""
        Var.watch[String](state, str => {
          symbol = str
        })

        state.set("abc")
        assert(symbol == "abc")
      }

      "should be cancelable" - {
        val state = Var.state[String]("123")

        var symbol = ""
        val watching = Var.watch[String](state, str => {
          symbol = str
        })

        watching.cancel()
        state.set("abc")

        assert(symbol == "123")
      }
    }

    "mapped var" - {
      "should be watchable" - {
        val st:State[String] = state("123")

        val mapped = st map (_.length)

        var capture = 0
        Var.watch[Int](mapped, int =>{
          capture = int
        })

        st.set("123456")
        assert(capture == 6)
      }

      "should stop notify when middle var not changed" - {
        val st:State[String] = state("123")
        val stringLength = st map (_.length)
        val nestVar = stringLength map (_ + 1)

        var notifyCounter = 0
        Var.watch[Int](nestVar, int =>{
          notifyCounter += 1
        })

        assert(notifyCounter == 1)

        st.set("12")
        assert(notifyCounter == 2)

        st.set("45")
        assert(notifyCounter == 2)

        st.set("456")
        assert(notifyCounter == 3)
      }
    }

    "zip var" -{
      "should only notify once at initial" -{
        val st1 = state("abc")
        val st2 = state(123)

        var counter = 0
        Var.watch[(String, Int)](st1 ~ st2, tup =>{
         counter += 1
        })
        assert(counter == 1)
      }
    }

  }
}
