import Mathlib.Data.BinaryHeap

namespace BinaryHeap

def contains {lt} [BEq α] (self : BinaryHeap α lt) (x : α) : Bool :=
  self.arr.contains x

end BinaryHeap