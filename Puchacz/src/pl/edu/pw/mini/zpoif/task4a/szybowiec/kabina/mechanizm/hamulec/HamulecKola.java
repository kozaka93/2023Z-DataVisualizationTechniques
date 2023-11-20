package pl.edu.pw.mini.zpoif.task4a.szybowiec.kabina.mechanizm.hamulec;

public class HamulecKola extends Hamulec {
	
	protected int stopienZahamowaniaPneumatyka;

	@Override
	public void hamuj(int procent) {
		stopienZahamowaniaPneumatyka += procent;
	}

	@Override
	public void odhamuj(int procent) {
		stopienZahamowaniaPneumatyka -= procent;
	}

}
