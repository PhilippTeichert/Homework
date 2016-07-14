package B06.A4;

import java.security.MessageDigest;
import java.util.Arrays;
 
import javax.crypto.Cipher;
import javax.crypto.spec.SecretKeySpec;
 
import sun.misc.BASE64Decoder;
import sun.misc.BASE64Encoder;

import KeyGenerator;

/**
 * Eine Klasse, die eine doppelte AES-Verschlüsselung durch einen Meet-In-The-Middle-Angriff knackt
 * gegeben einen Klartext und einen verschluesselten Schluesseltext
 * 
 * 
 * @author Alexander Gräsel, Louis Kobras, Utz Pöhlmann
 * Quelle: http://blog.axxg.de/java-aes-verschluesselung-mit-beispiel/
 */
public class AES {
 
	private final String _klartext = "Verschluesselung"; //Der Klartext
	private final String _schluesseltext ="be393d39ca4e18f41fa9d88a9d47a574"; //Der doppelt verschlüsselte Klartext
	private KeyGenerator _generator;
    /**
	 * Versucht, die beiden Schlüssel E_k1 und E_k2 heauszufinden mit
	 * _schluesseltext = E_k2(E_k1(_klartext))
	 * und scheibt sie auf die Konsole
	 * 
     * @param args
     * @throws Exception
     */
    public static void main(String[] args) throws Exception {
		AES aes = new AES();
		ArrayList<String> gefundeneSchluessel = aes.findeSchluessel(_klartext, _schluesselText);
		Syste.out.println(gefundeneSchluessel);
	}

    /**
	 * Versucht, die beiden Schlüssel E_k1 und E_k2 heauszufinden mit
	 * _schluesseltext = E_k2(E_k1(_klartext))
	 * durch einen Meet-In-The-Middle-Angriff
	 * 
	 * @param klartext
	 * @param schluesseltext
	 * @return eine ArrayList von Typ String mit folgenden Elementen:
	 * 		"Folgende Schluessel wurden gefunden:"
	 * 		"E_k1: " + schluessel1.toString()
	 * 		"E_k2: " + schluessel2.toString()
	 * 
	 */
	public ArrayList<String> findeSchluessel(String klartext, String schluesselText){
		ArrayList<SecretKeySpec> schluesselListe = generiereSchluessel();
		String textStufeEins1;
		String textStufeEins2;
		for(SecretKeySpec schluesselEnc : schluesselListe){
			textStufeEins1 = verschluesseln(klartext, schluesselEnc);
			for(SecretKeySpec schluesselDec : schluesselListe){
				textStufeEins2 = entschluesseln(schluesselText, schluesselDec);
				if(textStufeEins1.equals(textStufeEins2)){
					ArrayList<String> ergebnis = new ArrayList<String>;
					ergebnis.add("Folgende Schluessel wurden gefunden:");
					ergebnis.add("E_k1: " + schluesselEnc.toString());
					ergebnis.add("E_k2: " + schluesselDec.toString());
				}
			}
		}
		return ergebnis;
	}

	/**
	 * Generiert alle möglichen Schlüssel, auf die die Vorgaben (16 Byte, maximal an 2 Stellen keine 0-Bytes) zutreffen
	 * 
	 *  @return die Schluessel Liste als Liste von SecretKeySpec
	 */
	private ArrayList<SecretKeySpec> generiereSchluessel(){
		ArrayList<SecretKeySpec> raffinierteSchluesselListe = new ArrayList<SecretKeySpec>;
		_generator = new KeyGenerator();
		ArrayList<Key> schluesselListe = _generator.getKeys();
		for(Key schluessel : schluesselListe){
			SecretKeySpec schluessel = aes.bereiteSchluesselVor(key);
			raffinierteSchluesselListe.append(schluessel);
		}
		return raffinierteSchluesselListe;
	}
 
	/**
	 * Bringt einen Schluessel in ein Format mit dem das Programm weiter arbeiten kann (SecretKeySpec)
	 * 
	 * @param schluessel
	 * @return der fertige Schluessel als SecretKeySpec
	 */
	private SecretKeySpec bereiteSchluesselVor(Key schluessel){
		// Das Passwort bzw der Schluesseltext
		String schluesselString = schluessel.toString();
    	// byte-Array erzeugen
    	byte[] key = (schluesselString).getBytes("UTF-8");
    	// aus dem Array einen Hash-Wert erzeugen mit MD5 oder SHA
    	MessageDigest sha = MessageDigest.getInstance("SHA-256");
    	key = sha.digest(key);
    	// nur die ersten 128 bit nutzen
    	key = Arrays.copyOf(key, 16);
    	// der fertige Schluessel
    	return new SecretKeySpec(key, "AES");
	}
 
	/**
	 * Verschluesselt einen Klatext nach AES-Verfahren
	 * 
	 * @param klartext
	 * @return der verschluesselte Klartext
	 */
    private String verschluesseln(String klartext){
		// Verschluesseln
	    Cipher cipher = Cipher.getInstance("AES");
    	cipher.init(Cipher.ENCRYPT_MODE, secretKeySpec);
    	byte[] encrypted = cipher.doFinal(klartext.getBytes());

    	// bytes zu Base64-String konvertieren (dient der Lesbarkeit)
    	BASE64Encoder myEncoder = new BASE64Encoder();
    	return myEncoder.encode(encrypted);
	}
 
	/**
	 * Entschluesselt einen Klatext nach AES-Verfahren
	 * 
	 * @param schluesselText
	 * @return der entschluesselte schluesselText
	 */
    private String entschluesseln(String schluesselText){       
    	// BASE64 String zu Byte-Array konvertieren
    	BASE64Decoder myDecoder2 = new BASE64Decoder();
    	byte[] crypted2 = myDecoder2.decodeBuffer(geheim);
 
    	// Entschluesseln
    	Cipher cipher2 = Cipher.getInstance("AES");
    	cipher2.init(Cipher.DECRYPT_MODE, secretKeySpec);
    	byte[] cipherData2 = cipher2.doFinal(crypted2);
    	return new String(cipherData2);
	}
 
}
