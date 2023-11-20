package pl.edu.pw.mini.zpoif.task4a.szybowiec.kabina.inne;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;

public class Instruktor extends Pilot {

	public enum Uwaga {

		POCHYL_MASKE, NIE_PRZECHYLAJ, NIE_ZADZIERAJ, UWAZAJ_NA_OGON, TRZYMAJ_POZIOM, ZGLOS_GOTOWOSC, ZGLOS_PROSTA, ZGLOS_POZYCJE_Z_WIATREM, 
		PILNUJ_PREDKOSCI, WYPROWADZAJ, UCHYL_HAMULCE, SCHOWAJ_HAMULCE, JESTES_ZA_WYSOKO, JESTES_ZA_NISKO, TAK_NIE_DOLECISZ, TAK_TRZYMAJ, 
		DOBRZE, MOZE_BYC, WYCZEP_LINE, DELIKATNIEJ;

	}

	private List<Uwaga> uwagi = new ArrayList<>();

	public Instruktor() {
		prywatneMotto = "Nie gadac, latac!";
	}

	public Uwaga udzielUwagi(Boolean glosno) {
		Random random = new Random();
		Uwaga[] values = Uwaga.values();
		
		Uwaga uwaga = values[random.nextInt(values.length)];
		
		if(glosno) {
			System.out.println(uwaga + "!");
		}
		
		return uwaga;
	}

	public Uwaga udzielUwagi(Boolean glosno, Boolean zapamietaj) {
		
		Uwaga uwaga = udzielUwagi(glosno);
		
		if(zapamietaj) {
			uwagi.add(uwaga);
		}
		
		return uwaga;
	}

	public void wypiszPierwszeZapamietaneUwagi(int maxIlosc) {
		uwagi.stream().limit(maxIlosc).forEach(System.out::println);
	}
	
	public void zapomnijUwagi() {
		uwagi = new ArrayList<Instruktor.Uwaga>();
	}

}
