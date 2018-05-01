import React, { Component } from 'react';
import SlideOne from './SlideOne';
import SlideTwo from './SlideTwo';
import RightArrow from './RightArrow';
import LeftArrow from './LeftArrow';
require('./style.scss');

// The "parent" component
export default class Slider extends Component {
  constructor(props) {
    super(props);

    // The state object
    this.state = {
        // A property that will track what image should be displayed to the user
        slideCount: 1,
    };

    //Bind the functions in the constructor
    this.nextSlide = this.nextSlide.bind(this);
    this.previousSlide = this.previousSlide.bind(this);
  }

    // Function which sets the state to the next slide
    nextSlide() {
        this.setState({ slideCount: this.state.slideCount + 1 })
    }

    // Function which sets the state to the previous slide
    previousSlide() {
        this.setState({ slideCount: this.state.slideCount - 1 })
    }

    // Render function
    render() {
        return (
        <div className="slider">
            {/* Slides */}
            { this.state.slideCount === 1 ? <SlideOne /> : null }
            { this.state.slideCount === 2 ? <SlideTwo /> : null }

            {/* Arrow Functionality */}
            {/* Pass a prop to each arrow, the prop is a function */}
            <RightArrow nextSlide={this.nextSlide} />
            <LeftArrow previousSlide={this.previousSlide} />	
        </div>
        );
    }
}