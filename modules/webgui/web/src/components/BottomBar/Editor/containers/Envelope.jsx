import React, {Component} from 'react'
import PropTypes from 'prop-types';
import { connect } from 'react-redux';
import EnvelopeCanvas from '../presentational/EnvelopeCanvas'
import styles from '../style/Envelope.scss'
import GraphBody from '../../../common/Graph/GraphBody'
import Point from '../presentational/Point'
import PointPosition from '../presentational/PointPosition'
import { toggleActiveEnvelope, toggleActivePoint, movePoint, swapPoints } from '../../../../api/Actions/transferFunctionActions.js';

class Envelope extends Component {
  constructor(props) {
    super(props);
    this.handleDrag = this.handleDrag.bind(this);
    this.handleClick = this.handleClick.bind(this);
    this.checkForSwap = this.checkForSwap.bind(this);
    this.getPointPositions = this.getPointPositions.bind(this);
  }

  checkForSwap(position, points, index) {
    if (points[index].anchor)
      return -1;

    if (position.x < points[index - 1].position.x){
      return points[index - 1].anchor ? -1 : (index - 1);
    }
    else if (position.x > points[index + 1].position.x){
      return points[index + 1].anchor ? -1 : (index + 1);
    }
    else
      return -1;
  }

  handleDrag(e, ui, index, envelope) {
    var id = envelope.points[index].id;
    if(ui.deltaX !== 0 && ui.deltaY !== 0) {
      envelope.points[index].clickable = false;
    }

    let position = {
      x: envelope.points[index].position.x + ui.deltaX,
      y: envelope.points[index].position.y,
    }

    if(!envelope.points[index].anchor)
      position.y = position.y + ui.deltaY;

    this.props.MovePoint(position, id, envelope.id, this.props.URI);
    var swapMate = this.checkForSwap(position, envelope.points, index);

    if(swapMate !== -1) {
      this.props.SwapPoints(index, swapMate, envelope.id, this.props.URI);
    }
  }

  handleClick(envelope, pointId) {
    if (envelope.points[pointId].clickable === false) {
      envelope.points[pointId].clickable = true;
    }
    else {
      if (envelope.active === true) {
        this.props.TogglePoint(envelope.id, pointId, this.props.URI);
      }
      else if(this.hasActiveChild(envelope)) {
        this.props.TogglePoint(envelope.id, pointId, this.props.URI);
      }
      else {
        this.props.ToggleEnvelope(envelope.id, this.props.URI);
      }
    }
  }

  hasActiveChild(envelope) {
    var hasActiveChild = false;
    envelope.points.forEach(function(point) {
      if (point.active)
        hasActiveChild = true;
    })
    return hasActiveChild;
  }

  pointsToCanvas(points){
    return points.map((point, index) => ({
          ...point,
          position: {x: point.position.x - 10,
                    y: point.position.y - 10},
        })
    )
  }

  pointsForEnvelopeGraph(data){
    let convertedData = [];
    data.forEach(function(point) {
      let tmpObject = Object.assign({},
        {x: point.position.x ,
         y: 600 - point.position.y,
         color: point.color}
        )
      convertedData.push(tmpObject);
    })
    return convertedData;
  }

  getPointPositions(envelopes, height) {
    let convertedPoints = [];
    if(envelopes.length !== 0) {
      envelopes.map(envelope =>
          envelope.points.map(point =>
            convertedPoints.push(
              Object.assign({},
                {x1: point.position.x,
                 y1: point.position.y,
                 x2: point.position.x,
                 y2: height}
              )
            )
        )
      )
      return convertedPoints;
    }
  }

  render() {
    const { height, width, active, envelopes, minValue, maxValue, URI} = this.props;
    return (
      <div className={styles.Envelope}>
      {(envelopes.length !== 0) && (
        <div>
        <PointPosition className={styles.Envelope}
          points={this.getPointPositions(envelopes, height)}
          width={width}
          height={height}
          envelopes={envelopes}
          minValue={minValue}
          maxValue={maxValue}
        />
        {envelopes.map(envelope =>
          <div key={envelope.id}>
          <svg className={styles.Line} height={height} width={width + 10}>
            <GraphBody
             UseLinearGradient={true}
             points={this.pointsForEnvelopeGraph(envelope.points)}
             x={0}
             y={600}
             width={width}
             fillOpacity={"0"}
             strokeWidth={2}
            />
          </svg>
         {envelope.points.map((point, index) =>
          <Point className={styles.Envelope}
            key={point.id}
            handleClick={() => this.handleClick(envelope, point.id)}
            handleDrag={(e, ui) => this.handleDrag(e, ui, index, envelope)}
            height={height}
            width={width}
            {...point}
            bounds={{x1 : envelope.points[0].position.x, x2: envelope.points[envelope.points.length -1].position.x}}
            active={(point.active || envelope.active) ? true : false}
          />
          )}
         </div>
        )}
        </div>
      )}
    </div>
    );
  }
}
/*Envelope.propTypes = {
  points: PropTypes.arrayOf(
    PropTypes.shape({
      id: PropTypes.number.isRequired,
      position: PropTypes.shape({
              x: PropTypes.number.isRequired,
              y: PropTypes.number.isRequired,
            }).isRequired,
      anchor: PropTypes.bool.isRequired,
      color: PropTypes.string.isRequired,
    }).isRequired,
  ).isRequired,
  height: PropTypes.number.isRequired,
  width: PropTypes.number.isRequired,
  id: PropTypes.number.isRequired,
  active: PropTypes.bool.isRequired,
}*/

const mapStateToProps = (state, ownProps) => {
  let envelopes, URI, minValue, maxValue;
  state.sceneGraph.map(element => {
          if(element.name === "Enlil Sequence") {
            envelopes = element.subowners[1].subowners[1].properties[1].Value;
            URI = element.subowners[1].subowners[1].properties[1].Description.Identifier;
            console.log(element.subowners[1].subowners[1].properties[4].Value)
            console.log(element.subowners[1].subowners[1].properties[5].Value)
            minValue = Number(element.subowners[1].subowners[1].properties[4].Value);
            maxValue = Number(element.subowners[1].subowners[1].properties[5].Value);
          }
        })
    return {
      envelopes,
      URI,
      minValue,
      maxValue
    };
};

const mapDispatchToProps = (dispatch, ownProps) => {
  return {
    ToggleEnvelope: (id, URI) => {
      dispatch(toggleActiveEnvelope(id, URI));
    },
    TogglePoint: (envelopeId, pointId, URI) => {
      dispatch(toggleActivePoint(envelopeId, pointId, URI));
    },
    MovePoint: (position, id, envelopeId, URI) => {
      dispatch(movePoint(id, envelopeId, position, URI));
    },
    SwapPoints: ( id, swapId, envelopeId, URI) => {
      dispatch(swapPoints(id, swapId, envelopeId, URI));
    },
  }
}

Envelope = connect(
  mapStateToProps,
  mapDispatchToProps,
  )(Envelope)

export default Envelope;
