import React, { Component } from 'react';
import Slide from './Slide';
import RightArrow from './Arrows/RightArrow';
import LeftArrow from './Arrows/LeftArrow';
import Dots from './Dots/Dots';
import styles from './Slider.scss';

class Slider extends Component {
  constructor(props) {
    super(props);

    this.state = {
        index: 0,
        imagePaths: [],
    };

    //Bind the functions in the constructor
    this.getSliderImages = this.getSliderImages.bind(this);
    this.renderSlides = this.renderSlides.bind(this);
    this.nextSlide = this.nextSlide.bind(this);
    this.prevSlide = this.prevSlide.bind(this);
    this.handleDotClick = this.handleDotClick.bind(this);
  }

    componentDidMount() {
        this.getSliderImages();
    }
    
    getSliderImages() {
        // Import all files from a given directory path
        function importAll(r) {
            const images = [];
            r.keys().map((item) => { images[item.replace('./', '')] = r(item); });
            return images;
        }
        
        // Import all icon images from the given directory
        let stories = importAll(require.context('../../../../../../sync/url/stories/files', false, /\.(png|jpe?g|svg)$/));

        // Push images from stories object into images array
        for (var i = 0; i < Object.keys(stories).length; i++) { 
            this.state.imagePaths.push(stories[Object.keys(stories)[i]]);
        }
    }

    renderSlides() {
        // Map an imagePath to every Slide
        return (this.state.imagePaths.map((image, i) => {
            if(i === this.state.index){
            return ( <Slide 
                key={i} 
                image={this.state.imagePaths[Object.keys(this.state.imagePaths)[i]]} 
            />);
            }
        }))
    } 

    // Set the state to the next slide
    nextSlide() {
        this.setState({ index: this.state.index + 1 });
    }

    // Set the state to the previous slide
    prevSlide() {
        this.setState({ index: this.state.index - 1 });
    }

    // Handle the click of a dot
    handleDotClick(i) {
        const { index } = this.props;
        
        if(i === index) 
            return;
        else
            this.setState({ index: i });
      }

    render() {
        return (
        <div className={styles.Slider}>
            <div className={styles.SliderWrapper}>
                { this.renderSlides() }
            </div>
            <Dots 
                index={this.state.index}
                imagePaths={this.state.imagePaths}
                dotClick={this.handleDotClick}
            />
            { this.state.index < this.state.imagePaths.length-1 ? <RightArrow nextSlide={this.nextSlide} /> : null }
            { this.state.index > 0 ? <LeftArrow prevSlide={this.prevSlide} /> : null }
        </div>
        );
    }
}

export default Slider;