#include <gtest/gtest.h>

TEST(SquareRootTest, PositiveNos) {
  ASSERT_EQ(6, 2 * 3);
  ASSERT_EQ(6, -2 * -3);
}

TEST(SquareRootTest, NegativeNos) {
  ASSERT_EQ(-6, -2 * 3);
  ASSERT_EQ(-6, 2 * -3);
}

int main(int argc, char **argv) {
  testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
