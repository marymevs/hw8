#include <iostream>
using namespace std;

/*
 * The identity function.
 */
template <typename T>
auto Identity = [](T *x) { return x; };

/*
 * A generic List class.
 */
template <typename T>
class List {
public:
  T head;
  List<T> *tail;

  /*
   * Constructor
   */
  List (T hd, List<T> *tl) {
    head = hd;
    tail = tl;
  }

  /*
   * Prepend element hd to the list and return a new list.
   */
  List<T> *cons(T hd) {
    return new List<T>(hd, this);
  }

  /*
   * Print the list.
   */
  void print() {
    cout << head;
    if (tail) {
      cout << ",";
      tail->print();
    } else {
      cout << endl;
    }
  }

  /*
   * Static helper method that constructs a list from an array.
   */
  static List<T> *init(T *arr, int len) {
    List<T> *xs = NULL;
    for (int i = len - 1; i >= 0; i--) {
      xs = new List(arr[i], xs);
    }
    return xs;
  }
};

/*
 * Typedef for continuation type to make type definitions easier to read.
 */
typedef std::function<List<int>*(List<int>*)> Continuation;

/*
 * The inner 'loop' of bubble sort.
 */
List<int> *bubble_inner(List<int> *xs) {
  if (!xs || !xs->tail) {
    // list is empty or has only one element
    return xs;
  } else {
    // list has at least two elements
    int a = xs->head;
    int b = xs->tail->head;
    List<int> *rest = xs->tail->tail;
    if (a < b) {
      return bubble_inner(rest->cons(b))->cons(a);
    } else {
      return bubble_inner(rest->cons(a))->cons(b);
    }
  }
}

/*
 * The outer 'loop' of bubble sort.
 */
List<int> *bubble_outer(List<int> *xs) {
  if (!xs) {
    // list is empty; done
    return xs;
  } else {
    // list has an element;
    // recursively sort the rest of the list with bubble_outer
    // and then sort the result cons'ed with this element
    // using bubble_inner
    int x = xs->head;
    List<int> *xs2 = xs->tail;
    return bubble_inner(bubble_outer(xs2)->cons(x));
  }
}

/*
 * A wrapper bubblesort function; the user calls this one.
 */
List<int> *bubblesort(List<int> *xs) {
  return bubble_outer(xs);
}

int main() {
  // init list
  int arr[] = { 3, 7, 1, 0, 0, 45, 1001, 2, -100 };
  List<int> *xs = List<int>::init(arr, sizeof(arr)/(sizeof(*arr)));
  xs->print();

  // do sort
  List<int> *xs_sorted = bubblesort(xs);
  xs_sorted->print();
}
