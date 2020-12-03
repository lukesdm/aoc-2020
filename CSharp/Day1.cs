using System;
using System.IO;
using System.Linq;
using Xunit;
using Xunit.Abstractions;

namespace CSharp {
    public class Day1 {
        private readonly ITestOutputHelper _output;

        public Day1(ITestOutputHelper output) {
            _output = output;
        }

        private int solvePart1(int[] input) {
            foreach (var l in input) {
                foreach (var r in input) {
                    var sum = l + r;
                    if (sum == 2020) {
                        return l * r;
                    }
                }
            }

            throw new InvalidOperationException("No solution found.");
        }

        private int solvePart2(int[] input) {
            foreach (var a in input) {
                foreach (var b in input) {
                    foreach (var c in input) {
                        var sum = a + b + c;
                        if (sum == 2020) {
                            return a * b * c;
                        }
                    }
                }
            }

            throw new InvalidOperationException("No solution found.");
        }

        [Fact(DisplayName = "Part 1 - can solve for example")]
        public void Ex1() {
            // Arrange
            string[] input = {"1721", "979", "366", "299", "675", "1456"};
            int expected = 514579;

            // Act
            int actual = solvePart1(input.Select(int.Parse).ToArray());

            // Assert
            Assert.Equal(expected, actual);
        }

        [Fact]
        public void Part1_Actual() {
            var lines = File.ReadAllLines("day1.txt");
            var input = lines.Select(int.Parse).ToArray();

            var result = solvePart1(input);
            _output.WriteLine($"Part 1 result: {result}");

            Assert.True(true);
        }

        [Fact]
        public void Part2_Actual() {
            var lines = File.ReadAllLines("day1.txt");
            var input = lines.Select(int.Parse).ToArray();

            var result = solvePart2(input);
            _output.WriteLine($"Part 2 result: {result}");

            Assert.True(true);
        }
    }
}