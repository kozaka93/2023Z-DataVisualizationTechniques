package pl.edu.pw.mini.zpoif.task4a.szybowiec.kabina.inne;

import java.util.List;
import java.util.Random;

public abstract class Pilot {

	private static final List<String> imiona = List.of("Jan", "Michal", "Pawel", "Joanna", "Aneta", "Kamil");
	private static final List<String> nazwiska = List.of("Kowaluk", "Nowak", "Mielczarek", "Joziuk", "Gawel");

	protected String imie;
	protected String nazwisko;
	protected Nakolannik nakolannik;
	protected String prywatneMotto;

	public Pilot() {
		Random random = new Random();
		imie = imiona.get(random.nextInt(imiona.size()));
		nazwisko = nazwiska.get(random.nextInt(nazwiska.size()));
		nakolannik = new Nakolannik();
	}

	public Pilot(String imie, String nazwisko) {
		this.imie = imie;
		this.nazwisko = nazwisko;
	}

	public Pilot(String imie, String nazwisko, Nakolannik nakolannik) {
		super();
		this.imie = imie;
		this.nazwisko = nazwisko;
		this.nakolannik = nakolannik;
	}
	
	public void udzielUwagi() {
		
	}
	
}
