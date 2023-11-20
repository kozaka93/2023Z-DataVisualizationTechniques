package pl.edu.pw.mini.zpoif.task4a.szybowiec.kabina.uchwyt;

public abstract class Uchwyt {
	
	public enum Kolor {
		NIEBIESKI, ZOLTY, BRAZOWY
	}

	protected Kolor kolorUchwytu;

	protected Uchwyt(Kolor kolorUchwytu) {
		this.kolorUchwytu = kolorUchwytu;
	}
	
	public abstract void pociagnij(int wartosc);
	
	public abstract void popchnij(int wartosc);

	public Kolor getKolorUchwytu() {
		return kolorUchwytu;
	}
	
}
