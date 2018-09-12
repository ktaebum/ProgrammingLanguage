class Heap:
    """
    simple min heap implementation in python
    """

    def __init__(self):
        self.heap = []

    def __len__(self):
        return len(self.heap)

    @property
    def size(self):
        return len(self.heap)

    @size.setter
    def size(self, value):
        raise AttributeError('You cannot arbitrary modify heap size')

    def insert(self, x):
        self.heap.append(x)
        self._size += 1

    def get_min(self):
        return self.heap[0]

    def pop_min(self):
        result, self.heap[0] = self.heap[0], self.heap.pop(-1)

        return result

    def sift_up(self, i):
        while i > 0:
            parent = (i - 1) // 2

            if self.heap[parent] > self.heap[i]:
                self.heap[parent], self.heap[i] = self.heap[i],
                self.heap[parent]

            i = parent

    def sift_down(self, i):
        left = 2 * i + 1
        right = 2 * i + 2

        while left < len(self.heap):
            try:
                if self.heap[left] < self.heap[right]:
                    self.heap[i], self.heap[left] = self.heap[left],
                    self.heap[i]
                else:
                    self.heap[i], self.heap[right] = self.heap[right],
                    self.heap[i]
            except IndexError:
                # caught since right >= len(self.heap)
                pass
