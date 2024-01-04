let treeData = [];
let currentLevel = 0;
let coloringLevel = 0;
let isColoring = false;
let isOutlining = true;
let lights = [];
let decorations = [];

function setup() {
	let canvas = createCanvas(windowWidth * 0.9, windowHeight * 0.9);
	canvas.parent("canvas-holder");
	frameRate(10);
	updateTree();
}

function draw() {
	drawTree(treeData);

	if (!isColoring) {
		if (currentLevel < treeData.length) {
			currentLevel++;
		} else {
			isOutlining = false;
			isColoring = true;
		}
	} else {
		if (coloringLevel < treeData.length) {
			coloringLevel++;
		}
	}

	if (isColoring) {
		for (let light of lights) {
			if (random() < 0.05) {
				light.brightness = light.brightness === 255 ? random(100, 150) : 255;
			}
		}
	}
}

function updateTree() {
	clear();
	let size = int(document.getElementById("treeSize").value);
	treeData = [];

	for (let i = 0; i < size; i++) {
		treeData.push({
			level: size - i,
			width: (i + 1) * 50,
			color: "green",
		});
	}
	currentLevel = 0;
	coloringLevel = 0;
	isOutlining = true;
	isColoring = false;
	loop();
	generateDecorationsAndLights();
}

function drawTree(data) {
	translate(width / 2, height - 20);
	let overlap = 10;
	for (let i = data.length - 1; i >= 0; i--) {
		let level = data[i];
		let y = -level.level * 50;

		if (data.length - i <= currentLevel) {
			noFill();
			stroke(0);
			triangle(
				-level.width / 2,
				y,
				level.width / 2,
				y,
				0,
				y - 50 - overlap * i
			);
		}

		if (data.length - i <= coloringLevel) {
			let greenIntensity = map(level.level, 1, data.length, 60, 150);
			fill(0, greenIntensity, 0);
			noStroke();
			triangle(
				-level.width / 2,
				y,
				level.width / 2,
				y,
				0,
				y - 50 - overlap * i
			);
		}
	}

	for (let light of lights) {
		if (data.length - light.level <= coloringLevel) {
			fill(255, 255, 0, light.brightness);
			ellipse(light.x, light.y, 10, 10);
		}
	}

	if (data.length > 0 && coloringLevel >= data.length) {
		let topTriangle = data[0];
		let topTrianglePeakY = -topTriangle.level * 50;
		let starY = topTrianglePeakY - 60;
		fill(255, 215, 0);
		drawStar(0, starY, 15, 30, 5);
	}
}

function generateDecorationsAndLights() {
	lights = [];
	decorations = [];
	for (let i = 0; i < treeData.length; i++) {
		let numLights = random(3, 8);
		for (let j = 0; j < numLights; j++) {
			lights.push({
				level: i,
				x: random(-treeData[i].width / 2, treeData[i].width / 2),
				y: -treeData[i].level * 50 + random(-25, 25),
				brightness: 255,
			});
		}
	}
}

function drawStar(x, y, radius1, radius2, npoints) {
	let angle = TWO_PI / npoints;
	let halfAngle = angle / 2.0;
	beginShape();
	for (let a = 0; a < TWO_PI; a += angle) {
		let sx = x + cos(a) * radius2;
		let sy = y + sin(a) * radius2;
		vertex(sx, sy);
		sx = x + cos(a + halfAngle) * radius1;
		sy = y + sin(a + halfAngle) * radius1;
		vertex(sx, sy);
	}
	endShape(CLOSE);
}

function exportTree(format) {
	if (format === "png") {
		saveCanvas("choinka", "png");
	} else {
		console.log("Format not supported");
	}
}
