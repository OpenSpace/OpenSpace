import React, { Component } from 'react';
import { SketchPicker } from 'react-color';
import { connect } from 'react-redux';
import { changeColor } from './actions';

let createHandlers = function(dispatch) {
  let ChangeColor = function(data) {
    dispatch(changeColor(data))
  };
  return {
    ChangeColor,
  };
}

class ColorPic extends Component {
  constructor(props) {
    super(props);

    this.state = {
      background: '#fff',
    };

    this.handlers = createHandlers(this.props.dispatch);
}

  handleChangeComplete(color) {
    this.handlers.ChangeColor(color.hex);
  };

  render() {
    return (
      <SketchPicker
        {...this.props }
        color={ this.state.background }
        onChangeComplete={(color) => this.handleChangeComplete(color) }
      />
    );
  }
}
export default connect()(ColorPic);
