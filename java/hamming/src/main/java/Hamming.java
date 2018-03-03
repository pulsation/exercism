class Hamming {

    private final String leftStrand;
    private final String rightStrand;

    Hamming(String leftStrand, String rightStrand) {
      if (leftStrand.length() != rightStrand.length()) {
        throw new IllegalArgumentException("leftStrand and rightStrand must be of equal length.");
      }
      this.leftStrand = leftStrand;
      this.rightStrand = rightStrand;
    }

    int getHammingDistance() {
      int i = 0;
      int distance = 0;
      while (i < leftStrand.length()) {
        if (leftStrand.charAt(i) != rightStrand.charAt(i)) {
          distance++;
        }
        i++;
      }
      return distance;
    }

}
