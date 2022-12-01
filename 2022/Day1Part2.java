import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class Day1Part2 {
    public static void main(String[] args) throws FileNotFoundException {
        int[] top = new int[3];
        File file = new File("day1puzzleinput.txt");
        Scanner scanner = new Scanner(file);

        int curr = 0;
        while(scanner.hasNextLine()) {
            String line = scanner.nextLine();
            if (line == "") {
                if (curr > top[2] && curr < top[1]) {
                    top[2] = curr;
                }
                else if (curr > top[1] && curr < top[0]) {
                    top[2] = top[1];
                    top[1] = curr;
                }
                else if (curr > top[0]) {
                    top[2] = top[1];
                    top[1] = top[0];
                    top[0] = curr;
                }
                System.out.println(top[0] + ", " + top[1] + ", " + top[2]);
                curr = 0;
                continue;
            }
            curr += Integer.parseInt(line);
        }

        scanner.close();
        int sum = top[0] + top[1] + top[2];
        System.out.println(sum);
    }
}