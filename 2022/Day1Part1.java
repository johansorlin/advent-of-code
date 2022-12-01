import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class Day1Part1 {
    public static void main(String[] args) throws FileNotFoundException {
        File file = new File("day1puzzleinput.txt");
        Scanner scanner = new Scanner(file);

        int curr, most;
        curr = most = 0;
        while(scanner.hasNextLine()) {
            String line = scanner.nextLine();
            if (line == "") {
                if (curr > most) most = curr;
                curr = 0;
                continue;
            }
            curr += Integer.parseInt(line);
        }

        scanner.close();
        System.out.println(most);
    }
}