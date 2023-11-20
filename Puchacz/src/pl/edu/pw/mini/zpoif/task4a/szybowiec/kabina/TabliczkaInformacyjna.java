package pl.edu.pw.mini.zpoif.task4a.szybowiec.kabina;

import java.util.Random;

public class TabliczkaInformacyjna {

	private static final int predkoscDopuszczalna = 215;
	private static final int predkoscDopuszczalnaWAtmosferzeBurzliwej = 160;
	private static final int predkoscDopuszczalnaDlaManewrow = 150;
	private static final int predkoscDopuszczalnaOtwierania = 215;
	private static final int maksymalnaMasaStartowa = 570;
	private static final double maksymalnePrzeciazenie = 5.3d;
	private int nrSeryjny = new Random().nextInt(1000);
}
