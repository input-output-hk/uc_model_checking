
module common {

    import basicSpells.* from "../spells/basicSpells"    

    // ===== Definitions Common to All Functionalities =====

    type Time = int
    type Nonce = int
    type Party = int
    type SID = int
    type Counter = int    


    type MsgConfig = {
        party: Party,
        sid: SID,
        nonce: Nonce,
    }

  pure def listFilter(list: List[a], f: a => bool): List[a] = {
      list.foldl(List(), (r, e) =>   {
            if(f(e)) {
                r.append(e)
            } else {
                r
            }
      })
  }    

  pure def listHas(list: List[a], e: a): bool = {
      list.foldl(false, (r, x) => r or (x == e))
  }

  pure def setMax(a: Set[int],min: int): int = {
      a.fold(min, (r, x) => if (x > r) x else r)
  }

  pure def mySlice(l : List[a], start: int, end: int) : List[a] = {
      if (start >= end or start < 0 or end > l.length()) {
          List()
      } else {
          range(0,l.length()).foldl(List(), (r, i) => { 
              if (i >= start and i < end) {
                  r.append(l[i])
              } else {
                  r
              }
          })
      }
  }

  pure def setToList(__set: Set[a]): List[a] = {
      __set.fold(List(), (__l, __e) => __l.append(__e))
  }

  pure def listSamplingSpace(n : int, vals: Set[a]) : Set[List[a]] = {
      if (n <= 0) {
          Set(List())
      } else {
          // Build up from empty lists by iteratively adding each possible value
          // Start with a set containing one empty list, then for each position (1 to n),
          // expand each existing list by appending each possible value
          range(0, n).foldl(Set(List()), (currentLists, _) => {
              vals.fold(Set(), (newLists, v) => {
                  currentLists.fold(newLists, (acc, lst) => {
                      acc.union(Set(lst.append(v)))
                  })
              })
          })
      }
  }

  // all possible lists of length up to n consisting of values from vals
  pure def allUpToListSamplingSpace(n : int, vals: Set[a]) : Set[List[a]] = {
    0.to(n).fold(Set(), (acc, i) => {
      acc.union(listSamplingSpace(i, vals))
    })
  }

  pure def setMin (a: Set[int], max: int): int = {
      a.fold(max, (r, x) => if (x < r) x else r)
  }

  pure def listFilterMap(s: List[a], f: (a) => Option[b]): List[b] = {
    s.foldl(List(), (acc, e) => {
      match f(e) {
        | Some(x) => acc.append(x)
        | None => acc
      }
    })
  }

  pure def pickSome(inp : Set[a]) : a = {
    inp.find(x => true).unwrap()
  }    

}